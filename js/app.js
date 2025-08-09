/* ------------------------------------------------------------------ */
/*  GLOBAL STATE                                                      */
/* ------------------------------------------------------------------ */

let ws = null;

let currentUser        = null;
let userPoints         = 0;
let currentView        = 'challenges';
const solvedChallenges = new Set();
const revealedHints    = new Set();

let pingTimer;

/* Will be filled by loadChallenges() */
let challengesByCategory = {};
let challenges           = [];   // flat array convenience view

/* ------------------------------------------------------------------ */
/*  DOM CACHES                                                        */
/* ------------------------------------------------------------------ */
const loginPage      = document.getElementById('login-page');
const banner         = document.getElementById('banner');
const navigation     = document.getElementById('navigation');
const mainContent    = document.getElementById('main-content');
const challengeList  = document.getElementById('challenge-list');
const challengeDetail= document.getElementById('challenge-detail');
const scoreboardElm  = document.getElementById('scoreboard');

const asID = x => Number(x);

/* ------------------------------------------------------------------ */
/*  EVENT LISTENERS                                                   */
/* ------------------------------------------------------------------ */
document.getElementById('login-form')   .addEventListener('submit', handleLogin);
document.getElementById('challenges-btn').addEventListener('click', () => {
    showView('challenges', true);
});
document.getElementById('scoreboard-btn').addEventListener('click', () => showView('scoreboard', true));
document.getElementById('logout-btn')   .addEventListener('click', handleLogout);
document.getElementById('back-btn')     .addEventListener('click', () => showView('challenges', true));


/* ------------------------------------------------------------------ */
/*  ERRORS
    /* ------------------------------------------------------------------ */
/**
 * Show a red notification banner.
 * @param {string} message  What to display
 * @param {number} [ms=5000]  How long before it fades away (0 → stay until closed)
 */
function showError(message, ms = 5000) {
    // 1. Ensure we have a host container.
    let host = document.getElementById('error-host');
    if (!host) {
        host = document.createElement('div');
        host.id = 'error-host';
        host.className = 'fixed top-4 right-4 space-y-2 z-50';
        document.body.appendChild(host);
    }

    // 2. Build one error card.
    const card = document.createElement('div');
    card.className =
        'bg-red-600 text-white px-4 py-3 rounded shadow-lg flex items-start gap-3 animate-[slide-in_.2s_ease-out]';
    card.innerHTML = `
      <span class="flex-1 text-sm">${message}</span>
      <button class="font-bold ml-3 hover:text-red-200 focus:outline-none">&times;</button>
  `;

    // 3. Close button behaviour.
    card.querySelector('button').addEventListener('click', () => card.remove());

    host.appendChild(card);

    // 4. Auto-dismiss, if requested.
    if (ms > 0) {
        setTimeout(() => card.remove(), ms);
    }
}


/* ------------------------------------------------------------------ */
/*  LOGIN / LOGOUT                                                    */
/* ------------------------------------------------------------------ */

/* -------------------- websocket helper -------------------- */
const BACKOFF_MIN  = 1_000;     // 1 s
const BACKOFF_MAX  = 30_000;    // 30 s

let reconnectDelay = BACKOFF_MIN;

/* ------------------------------------------------------------------ */
/*  WEBSOCKET HELPERS                                                 */
/* ------------------------------------------------------------------ */

/* ────────── 1 ▸ low-level data mutator ────────── */
function applyScoreDelta (msg) {
    const { ts:timestamp, displayname:name, points:delta } = msg;
    if (timestamp == null || name == null || delta == null) return;

    /*  A. running totals  */
    const newTotal = (scoreboard.get(name) || 0) + delta;
    scoreboard.set(name, newTotal);

    /*  B. timeline        */
    const tl = timelines.get(name) ?? [];
    tl.push({ x: timestamp, y: newTotal, challenge: msg.challenge });
    tl.sort((a, b) => a.x - b.x);          // keep in order
    timelines.set(name, tl);
}

/* ────────── 2 ▸ expensive repaint, called ONCE ────────── */
function flushChart () {
    /* rebuild datasets, same logic as before … */
    const topUsers = [...scoreboard.entries()]
          .sort((a,b) => b[1]-a[1]).slice(0, TOP_N).map(([u]) => u);

    scoreChart.data.datasets = topUsers.map((u, i) => ({
        label: u,
        data : timelines.get(u),
        borderColor    : scoreChart.data.datasets.find(d=>d.label===u)?.borderColor
            ?? palette[i % palette.length],
        backgroundColor: palette[i % palette.length],
        pointRadius    : 3,
        tension        : .3,
        borderWidth    : 2
    }));

    scoreChart.update('none');
    refreshScoreboard();
}

/* ────────── 3 ▸ message handler split ────────── */
async function handleScoreEvent (msg, deferFlush = false) {
    if (seenEventIDs.has(msg.id)) return;
    seenEventIDs.add(msg.id);

    /* We handle scores and hints here. */
    if (msg.type === 'score') {
        if (msg.displayname === currentUser) {
            solvedChallenges.add(asID(msg.challenge_id));
            if (msg.reload === 'true') await loadChallenges();
            if (currentView === 'challenges') renderChallenges();
            else if (!challengeDetail.classList.contains('hidden')
                     && openChallengeId === msg.challenge_id)
                showChallenge(msg.challenge_id);
        }
    }

    applyScoreDelta(msg);          // always cheap
    userPoints = (scoreboard.get(currentUser) || 0); /* + msg.points; */
    document.getElementById('user-points').textContent = `${userPoints} pts`;
    if (!deferFlush) flushChart(); // expensive only if we ask for it
}

function dispatchEvent (msg, defer=false) {
    switch (msg.type) {
    case 'hint':
        handleScoreEvent(msg, defer);
        if (msg.displayname === currentUser) {      // ★ NEW
            /* our own hint purchase → refresh data once */
            loadChallenges().then(() => {
                if (openChallengeId === msg.challenge_id)
                    showChallenge(openChallengeId);
            });
        }
        break;
    default:
        handleScoreEvent(msg, defer);
    }
}

/* --- 2.  Connect & normalise payload to an array ------------------ */
function connectWS (websocket_url) {
    if (!currentUser) return;

    ws = new WebSocket(websocket_url);

    ws.addEventListener('open', () => {
        console.log('socket open');
        reconnectDelay = BACKOFF_MIN;
    });

    ws.addEventListener('message', e => {
        let payload;
        try      { payload = JSON.parse(e.data); }
        catch(err){ console.error('Bad WS JSON', err); return; }

        if (Array.isArray(payload)) {
            /* initial dump or server batch ─ mutate quickly … */
            payload.forEach(m => handleScoreEvent(m, /*deferFlush=*/true));
            /* …then one single repaint */
            flushChart();
        } else {
            dispatchEvent(payload);
        }
    });

    ws.addEventListener('close', e => {
        console.log('WS closed', {code: e.code, reason: e.reason, wasClean: e.wasClean});
        clearInterval(pingTimer);
        const delay = reconnectDelay + Math.random() * 500;
        setTimeout(() => connectWS(websocket_url), delay);
        reconnectDelay = Math.min(reconnectDelay * 2, BACKOFF_MAX);
    });

    ws.addEventListener('error', err => {
        console.error('socket error', err);
        ws.close();
    });

    /* Send pings every 45s to keep connection alive. */
    clearInterval(pingTimer);
    pingTimer = setInterval(() => {
      if (ws.readyState === WebSocket.OPEN) {
        ws.send(JSON.stringify({ type: 'ping' }));
      }
    }, 45_000);  // 45 s = 45 000 ms
}

function bootFromLocationHash(ujson) {
    const h = location.hash.slice(1);          // drop the “#”

    if (h === 'scoreboard') {
        finishLogin(ujson);
        showView('scoreboard');                  // no push
    } else if (h.startsWith('challenge-')) {
        const id = Number(h.split('-')[1]);
        finishLogin(ujson);
        showChallenge(id);                       // no push
    } else {
        finishLogin(ujson);                    // grid by default
    }
}

function finishLogin ({ username, displayname, needs_name, websocket_url }) {

    currentUser = displayname || username;
    window.currentUser = username;
    document.getElementById('user-name').textContent =
        displayname || '(unnamed)';

    showView('challenges');

    /* first-login onboarding */
    if (needs_name) {
        showNameModal();
        document.getElementById('name-form')
            .addEventListener('submit', handleNameSubmit, { once: true });
    }

    console.log('banner element:', banner);

    loginPage.classList.add('hidden');
    banner.classList.remove('hidden');
    navigation.classList.remove('hidden');
    mainContent.classList.remove('hidden');

    loadChallenges();

    showView('challenges');   // render grid
    history.replaceState({ view:'challenges' }, '', '#challenges');

    connectWS(websocket_url);
}

function showLogin() {
    loginPage.classList.remove('hidden');
    banner.classList.add('hidden');
    navigation.classList.add('hidden');
    mainContent.classList.add('hidden');
}

async function handleLogin(e) {
    e.preventDefault();

    const username = document.getElementById('username').value.trim();
    const password = document.getElementById('password').value;

    if (!username || !password) return;

    try {
        /* ------------ call the server ------------ */
        const res = await fetch('/api/login', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ username, password })
        });

        if (res.status === 401) {
            showError('Invalid credentials');             // your own helper
            return;
        }
        if (!res.ok) throw new Error(`HTTP ${res.status}`);

        const { displayname, needs_name, websocket_url } = await res.json();

        bootFromLocationHash({ username, displayname, needs_name, websocket_url });

    } catch (err) {
        console.error(err);
        showError('Could not reach the server. Try again later.');
    }
}

async function handleLogout () {
    /* 1 ─ ask server to kill the session */
    try {
        await fetch('/api/logout', {
            method     : 'POST',
            credentials: 'include'
        });
    } catch (err) {
        console.warn('Logout call failed (offline?)', err);
    }

    /* 2 ─ close websocket (if open) */
    if (ws && ws.readyState === WebSocket.OPEN) ws.close();
    ws = null;

    /* 3 ─ wipe *all* in-memory state */
    currentUser  = null;
    userPoints   = 0;
    openChallengeId = null;

    solvedChallenges.clear();
    revealedHints.clear();
    seenEventIDs.clear();
    challengesByCategory = {};
    challenges           = [];

    timelines.clear();
    scoreboard.clear();

    /* 4 ─ reset Chart.js datasets */
    if (scoreChart) {
        scoreChart.data.datasets = [];
        scoreChart.update('none');
    }

    /* 5 ─ clear dynamic UI fragments */
    document.getElementById('user-name').textContent   = '';
    document.getElementById('user-points').textContent = '0 pts';
    document.getElementById('challenges-grid').innerHTML = '';
    document.getElementById('scoreboard-body').innerHTML = '';

    /* hide any modal that might still be open */
    hideNameModal?.();

    /* 6 ─ show login screen & blank form */
    showLogin();
    document.getElementById('username').value = '';
    document.getElementById('password').value = '';
}

/* ------------------------------------------------------------------ */
/*  VIEW MANAGEMENT                                                   */
/* ------------------------------------------------------------------ */
function showView(view, push = false) {
    currentView = view;

    /* nav button styling */
    document.querySelectorAll('.nav-btn').forEach(btn => {
        btn.classList.remove('active', 'bg-gradient-to-r', 'from-green-500', 'to-blue-500', 'text-white');
        btn.classList.add   ('text-slate-300', 'hover:text-white', 'hover:bg-slate-700');
    });

    /* hide all sections */
    challengeList .classList.add('hidden');
    challengeDetail.classList.add('hidden');
    scoreboardElm .classList.add('hidden');

    if (view === 'challenges') {
        document.getElementById('challenges-btn').classList.remove('text-slate-300', 'hover:text-white', 'hover:bg-slate-700');
        document.getElementById('challenges-btn').classList.add   ('active', 'bg-gradient-to-r', 'from-green-500', 'to-blue-500', 'text-white');
        challengeList.classList.remove('hidden');
        renderChallenges();               // uses global challengesByCategory
    } else if (view === 'scoreboard') {
        document.getElementById('scoreboard-btn').classList.remove('text-slate-300', 'hover:text-white', 'hover:bg-slate-700');
        document.getElementById('scoreboard-btn').classList.add   ('active', 'bg-gradient-to-r', 'from-green-500', 'to-blue-500', 'text-white');
        scoreboardElm.classList.remove('hidden');
        renderScoreboard();
    }

    /* ── history ────────────────────────────────────────────── */
    if (push) {
        history.pushState({ view }, '', `#${view}`);
    }
}

/* ------------------------------------------------------------------ */
/*  CHALLENGE GRID RENDERING                                          */
/* ------------------------------------------------------------------ */
function renderChallenges() {
    const container = document.getElementById('challenges-grid');
    container.innerHTML = '';

    if (Object.keys(challengesByCategory).length === 0) {
        container.innerHTML =
            '<p class="text-slate-400">No challenges loaded. Please try again later.</p>';
        return;
    }

    Object.entries(challengesByCategory).forEach(([categoryName, categoryItems]) => {
        /* category wrapper */
        const categorySection = document.createElement('div');
        categorySection.className = 'col-span-full mb-8';

        /* header */
        const categoryHeader = document.createElement('div');
        categoryHeader.className = 'flex items-center gap-3 mb-6';
        categoryHeader.innerHTML = `
      <div class="w-8 h-8 ${getCategoryIconBg(categoryName)} rounded-lg flex items-center justify-center">
        ${getCategoryIcon(categoryName)}
      </div>
      <h2 class="text-2xl font-bold text-white">${categoryName}</h2>
      <div class="flex-1 h-px bg-gradient-to-r from-slate-600 to-transparent"></div>
      <span class="text-slate-400 text-sm">
        ${categoryItems.length} challenge${categoryItems.length !== 1 ? 's' : ''}
      </span>
    `;

        /* card grid */
        const categoryGrid = document.createElement('div');
        categoryGrid.className = 'grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6';

        // inside renderChallenges() … 
        categoryItems.forEach(challenge => {
            const isSolved = solvedChallenges.has(asID(challenge.id));

            /* --- 1. decide colours ------------------------------------------------ */
            const base      = 'rounded-lg p-6 transition-colors cursor-pointer transform hover:scale-105 duration-200';
            const solvedClr = 'bg-green-800/40 border-green-500 hover:border-green-400';
            const unsolved  = 'bg-slate-800/50 border-slate-700 hover:border-green-400';

            /* --- 2. create card ---------------------------------------------------- */
            const card = document.createElement('div');
            card.className = `${base} ${isSolved ? solvedClr : unsolved}`;

            card.innerHTML = `
    <div class="flex items-start justify-between mb-4">
      <div>
        <h3 class="text-xl font-semibold text-white mb-2">${challenge.title}</h3>
      </div>
          ${isSolved ? '<div class="text-green-400"><svg class="w-6 h-6" fill="currentColor" viewBox="0 0 20 20"><path fill-rule="evenodd" d="M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z" clip-rule="evenodd"></path></svg></div>' : ''}
    </div>
    <div class="flex items-center justify-between">
      <span class="text-${getDifficultyColor(challenge.difficulty)}-400 text-sm font-medium">
        ${challenge.difficulty}
      </span>
      <span class="text-green-400 font-semibold">${challenge.points} pts</span>
    </div>
  `;

            card.addEventListener('click', () => showChallenge(challenge.id, true));
            categoryGrid.appendChild(card);
        });

        categorySection.appendChild(categoryHeader);
        categorySection.appendChild(categoryGrid);
        container.appendChild(categorySection);
    });
}

/* ------------------------------------------------------------------ */
/*  ICON / COLOR HELPERS                                              */
/* ------------------------------------------------------------------ */
function getCategoryIcon(category) {
    const icons = {
        'Web'                : '<svg class="w-4 h-4 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 12a9 9 0 01-9 9m9-9a9 9 0 00-9-9m9 9H3m9 9v-9m0-9v9m0 9c-5 0-9-4-9-9s4-9 9-9"></path></svg>',
        'Binary Exploitation': '<svg class="w-4 h-4 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 9l3 3-3 3m5 0h3M5 20h14a2 2 0 002-2V6a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z"></path></svg>',
        'Cryptography'       : '<svg class="w-4 h-4 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z"></path></svg>',
        'Forensics'          : '<svg class="w-4 h-4 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"></path></svg>',
        'Reverse Engineering': '<svg class="w-4 h-4 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"></path></svg>'
    };
    return icons[category] || '<svg class="w-4 h-4 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 21a4 4 0 01-4-4V5a2 2 0 012-2h4a2 2 0 012 2v12a4 4 0 01-4 4zm0 0h12a2 2 0 002-2v-4a2 2 0 00-2-2h-2.343M11 7.343l1.657-1.657a2 2 0 012.828 0l2.829 2.829a2 2 0 010 2.828l-8.486 8.485M7 17h.01"></path></svg>';
}

function getCategoryIconBg(category) {
    const colors = {
        'Web'                : 'bg-blue-500',
        'Cryptography'       : 'bg-purple-500',
        'Binary Exploitation': 'bg-red-500',
        'Forensics'          : 'bg-yellow-500',
        'Reverse Engineering': 'bg-green-500'
    };
    return colors[category] || 'bg-gray-500';
}

function getCategoryColor(category) {
    const colors = {
        'Web'                : 'bg-blue-500/20 text-blue-400',
        'Cryptography'       : 'bg-purple-500/20 text-purple-400',
        'Binary Exploitation': 'bg-red-500/20 text-red-400',
        'Forensics'          : 'bg-yellow-500/20 text-yellow-400',
        'Reverse Engineering': 'bg-green-500/20 text-green-400'
    };
    return colors[category] || 'bg-gray-500/20 text-gray-400';
}

function getDifficultyColor(diff) {
    return { Easy: 'green', Medium: 'yellow', Hard: 'red' }[diff] || 'gray';
}

/* ------------------------------------------------------------------ */
/*  CHALLENGE DETAIL & FLAG SUBMIT                                    */
/* ------------------------------------------------------------------ */
let openChallengeId = null;

/* ─── helpers ────────────────────────────────────────────────────── */

function lastUpdateTs (name) {
  const tl = timelines.get(name);
  return tl && tl.length ? tl[tl.length - 1].x          // last .x is latest
                         : Number.POSITIVE_INFINITY;    // player has 0 pts
}

function generateHintHTML (chID, hint, owned, active) {

    console.log(chID);
    console.log(hint);
    console.log(owned);
    console.log(active);

    if (owned) {
        /* text is present because `/api/challenges` included it */
        return `
      <div class="my-2 p-4 border border-slate-700 rounded-lg">
        <p class="text-slate-300 text-sm">${hint.text}</p>
      </div>`;
    }

    if (active) {
        return `
      <div class="my-2 p-4 border border-slate-700 rounded-lg flex justify-between items-center">
        <span class="text-slate-300 text-sm">
          Hint (<span class="text-yellow-400 font-semibold">${hint.cost} pts</span>)
        </span>
        <button
          class="reveal-hint-btn bg-purple-600 hover:bg-purple-700 px-3 py-1 rounded"
          data-ch="${chID}" data-hint="${hint.id}">
          Reveal
        </button>
      </div>`;
    }

    /* locked */
    return `
    <div class="my-2 p-4 border border-slate-700 rounded-lg opacity-30 cursor-not-allowed">
      <em class="text-slate-400">Locked – reveal earlier hints first</em>
    </div>`;
}

async function handleRevealHint (e) {
    const btn    = e.currentTarget;
    const chID   = Number(btn.dataset.ch);
    const hintID = Number(btn.dataset.hint);

    btn.disabled = true;            // prevents double-clicks
    btn.textContent = '…';

    try {
        const res = await fetch('/api/hint', {
            method      : 'POST',
            credentials : 'include',
            headers     : { 'Content-Type': 'application/json' },
            body        : JSON.stringify({ id: chID, hint_id: hintID })
        });

        if (res.status === 401) { showError('Session expired'); return showLogin(); }
        const { result, message } = await res.json();
        if (result !== 'ok') throw new Error(message || 'Not enough points');

        await loadChallenges();
        showChallenge(chID);                // re-render with hint text
    } catch (err) {
        console.error(err);
        showError(err.message || 'Could not reveal hint');
        btn.disabled = false;          // allow retry
        btn.textContent = 'Reveal';
    }
}

function showChallenge(challengeId, push = false) {
    if (push) {
        history.pushState({ view: 'challenge', id: challengeId },
                          '', `#challenge-${challengeId}`);
    }

    openChallengeId = challengeId;
    const challenge = challenges.find(c => c.id === challengeId);
    if (!challenge) return;

    const isSolved = solvedChallenges.has(asID(challengeId));

    challengeList .classList.add   ('hidden');
    challengeDetail.classList.remove('hidden');

    document.getElementById('challenge-content').innerHTML = generateChallengeHTML(challenge, isSolved);

    document.querySelectorAll('.reveal-hint-btn').forEach(btn =>
        btn.addEventListener('click', handleRevealHint));

    if (!isSolved) {
        document.getElementById(`flag-form-${challengeId}`)
            .addEventListener('submit', e => handleFlagSubmit(e, challengeId));
    }
}

async function handleFlagSubmit(e, challengeId) {
    e.preventDefault();

    console.log("handleFlagSubmit");

    const flagInput = document.getElementById(`flag-input-${challengeId}`);
    const resultDiv = document.getElementById(`flag-result-${challengeId}`);
    const submitted = flagInput.value.trim();

    console.log(submitted);

    if (!submitted) return;

    try {
        const res = await fetch('/api/submit', {
            method:       'POST',
            credentials:  'include',              // send the session cookie
            headers:      { 'Content-Type': 'application/json' },
            body:         JSON.stringify({ id: challengeId, flag: submitted })
        });

        if (res.status === 401) {           // session expired
            showError('Please log in again');  // your red-banner helper
            return showLogin();               // switch UI back to login view
        }

        const data = await res.json();
        console.log(data)
        if (data.result === 'correct') {
            solvedChallenges.add(asID(challengeId));
            loadChallenges();
            renderChallenges();
            resultDiv.innerHTML = successHtml(data.points);
            setTimeout(() => showChallenge(challengeId), 1500);
        } else if (data.result === 'already') {
            resultDiv.innerHTML = `<div class="text-green-400">Already solved!</div>`;
        } else {
            resultDiv.innerHTML = errorHtml('Incorrect flag. Try again!');
        }
    } catch (err) {
        console.error(err);
        showError('Network error — try again');
    } finally {
        flagInput.value = '';
    }
}

/* tiny helpers for cleaner markup */
function successHtml(pts) {
    return `
    <div class="flex items-center gap-2 text-green-400">
      <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 20 20">
        <path fill-rule="evenodd"
          d="M16.707 5.293a1 1 0 011.414 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z"
          clip-rule="evenodd"></path>
      </svg>
      <span class="font-semibold">Correct! +${pts} points</span>
    </div>`;
}

function errorHtml(msg) {
    return `
    <div class="flex items-center gap-2 text-red-400">
      <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 20 20">
        <path fill-rule="evenodd"
          d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
          clip-rule="evenodd"></path>
      </svg>
      <span>${msg}</span>
    </div>`;
}

function generateChallengeHTML(ch, isSolved) {
    return `
    <div class="bg-slate-800/50 border border-slate-700 rounded-lg p-8">
      <div class="flex items-start justify-between mb-6">
        <div>
          <h1 class="text-3xl font-bold text-white mb-2">${ch.title}</h1>
          <div class="flex items-center gap-4">
            <span class="inline-block px-3 py-1 text-sm font-medium rounded-full ${getCategoryColor(ch.category)}">
              ${ch.category}
            </span>
            <span class="text-${getDifficultyColor(ch.difficulty)}-400 text-sm font-medium">${ch.difficulty}</span>
            <span class="text-green-400 font-semibold">${ch.points} pts</span>
            ${isSolved ? '<span class="text-green-400 flex items-center gap-1"><svg class="w-4 h-4" fill="currentColor" viewBox="0 0 20 20"><path fill-rule="evenodd" d="M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z" clip-rule="evenodd"></path></svg>Solved</span>' : ''}
          </div>
        </div>
      </div>

      <div class="mb-6">
        <h2 class="text-xl font-semibold text-white mb-3">Description</h2>
      <div class="px-6 pb-2 text-white/75" style="text-wrap:balance">${marked.parse(ch.description)}</div>

      </div>

      ${
        ch.hints?.length
          ? `<div class="mb-6">
               <h2 class="text-xl font-semibold text-white mb-3">Hints</h2>
               ${
                 (() => {
                   let unlockedGiven = false;
                     return ch.hints.map(h => {
                         console.log("-------------------------");
                         console.log(h);
                     const owned  = revealedHints.has(`${ch.id}:${h.id}`);
                     const active = !owned && !unlockedGiven;
                     if (active) unlockedGiven = true;
                     return generateHintHTML(ch.id, h, owned, active);
                   }).join('');
                 })()
               }
             </div>`
          : ''                   // ← nothing rendered if there are no hints
      }

      ${!isSolved ? `
        <div class="border-t border-slate-700 pt-6">
        <h2 class="text-xl font-semibold text-white mb-4">Submit Flag</h2>
        <form id="flag-form-${ch.id}" class="flex gap-3">
        <input type="text" id="flag-input-${ch.id}" placeholder="Enter FLAG text here"
    class="flex-1 h-10 rounded-md border border-slate-600 bg-slate-700/50 px-3 py-2 text-white placeholder:text-slate-400 focus:border-green-400 focus:outline-none focus:ring-2 focus:ring-green-400" />
        <button type="submit"
    class="px-6 py-2 bg-gradient-to-r from-green-500 to-blue-500 hover:from-green-600 hover:to-blue-600 text-white font-semibold rounded-lg transition-all">
        Submit
    </button>
        </form>
        <div id="flag-result-${ch.id}" class="mt-3"></div>
        </div>` : `
        <div class="border-t border-slate-700 pt-6">
        <div class="flex items-center gap-3 text-green-400">
        <svg class="w-6 h-6" fill="currentColor" viewBox="0 0 20 20">
        <path fill-rule="evenodd" d="M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z" clip-rule="evenodd"></path>
        </svg>
        <span class="text-lg font-semibold">Challenge Solved!</span>
        <span class="text-sm">+${ch.points} points</span>
        </div>
        </div>`}
    </div>`;
}

/* ------------------------------------------------------------------ */
/*  SCOREBOARD                                                        */
/* ------------------------------------------------------------------ */
function renderScoreboard () {
    const tbody = document.getElementById('scoreboard-body');
    tbody.innerHTML = '';

    /* 1 ─ build an array from the scoreboard map */
    const rows = [...scoreboard.entries()].map(([name, score]) => ({
        name,
        score,
        solved : (timelines.get(name)?.length || 0),
        ts     : lastUpdateTs(name)
    }));

    /* 2 ─ make sure the current player shows up even with 0 pts */
    if (currentUser && !scoreboard.has(currentUser)) {
        rows.push({ name: currentUser, score: userPoints, solved: solvedChallenges.size });
    }

    /* 3 ─ rank & render */
    rows.sort((a, b) => {
        if (b.score !== a.score) return b.score - a.score; // ① score desc
        return a.ts - b.ts;                                // ② earlier first
    })
        .forEach((e, i) => {
            const tr = document.createElement('tr');
            tr.className = `border-b border-slate-700 ${e.name === currentUser ? 'bg-green-900/20' : ''}`;
            tr.innerHTML = `
          <td class="px-6 py-4 text-slate-300">#${i + 1}</td>
          <td class="px-6 py-4 text-white font-medium">${e.name}</td>
          <td class="px-6 py-4 text-right text-green-400 font-semibold">${e.score}</td>
          <td class="px-6 py-4 text-right text-slate-300">${e.solved}</td>`;
            tbody.appendChild(tr);
        });
}

/* ------------------------------------------------------------------ */
/*  DATA FETCH                                                        */
/* ------------------------------------------------------------------ */
async function loadChallenges() {
    try {
        const res = await fetch('/api/challenges', { cache: 'no-store' });
        if (!res.ok) throw new Error(`HTTP ${res.status}`);

        const data = await res.json();

        /* Accept either a grouped object (preferred) or a flat array */
        if (Array.isArray(data)) {
            challengesByCategory = data.reduce((acc, c) => {
                (acc[c.category] ||= []).push(c); return acc;
            }, {});
        } else {
            challengesByCategory = data;
        }

        Object.values(challengesByCategory).forEach(list =>
            list.sort((a, b) => Number(a.id) - Number(b.id)));

        challenges = Object.values(challengesByCategory).flat();

        revealedHints.clear();
        challenges.forEach(ch =>
            ch.hints?.forEach(h => {
                console.log(h);
                /* Convention: server includes `text` (or `revealed:true`)      */
                /* only when the player has bought the hint                     */
                if (h.text) revealedHints.add(`${ch.id}:${h.id}`);
            }));

        console.log(challenges);

        if (currentView === 'challenges') renderChallenges();
    } catch (err) {
        console.error('Failed to load challenges:', err);
        document.getElementById('challenges-grid').innerHTML =
            '<p class="text-red-400">Could not load challenges. Try again later.</p>';
    }
}

/* --------------------- config ------------------------------ */
const TOP_N = 10;
const palette = [
    '#60a5fa','#f87171','#34d399','#e879f9','#fbbf24',
    '#a78bfa','#fb923c','#4ade80','#f472b6','#38bdf8'
];

/* --------------------- local caches ------------------------ */
const timelines = new Map();          // username → [{x, y}, …]
const scoreboard = new Map();         // username → latest points
const seenEventIDs = new Set();
let scoreChart;                       // Chart.js instance

function initChart () {
    const ctx = document.getElementById('score-chart').getContext('2d');
    scoreChart = new Chart(ctx, {
        type: 'line',
        data: { datasets: [] },
        options: {
            animation: true,
            responsive: true,
            maintainAspectRatio: false,
            plugins: { legend: { labels: { color: '#e2e8f0' } },
                       tooltip: {
                           callbacks: {
                               // ① main line (“Alice: 120 pts”)
                               label(ctx) {
                                   return `${ctx.dataset.label}: ${ctx.parsed.y} pts`;
                               },
                               // ② extra line with the challenge, only if present
                               afterLabel(ctx) {
                                   const title = ctx.raw.challenge;
                                   return title ? `🏁 ${title}` : '';   // empty string = no line
                               }
                           }
                       }
                     },
            interaction: { mode: 'nearest', intersect: false },

            scales: {
                x: {
                    type: 'time',
                    time: { tooltipFormat: 'PPpp' },
                    ticks: { color: '#cbd5e1' },
                    grid : { color: 'rgba(148,163,184,0.1)' }
                },
                y: {
                    beginAtZero: true,
                    ticks: { color: '#cbd5e1' },
                    grid : { color: 'rgba(148,163,184,0.1)' }
                }
            }
        }
    });
}

function refreshScoreboard () {
    const tbody = document.getElementById('scoreboard-body');
    tbody.innerHTML = '';                       // clear

    const rows = [...scoreboard.entries()]
          .map(([name, score]) => ({ name, score, ts: lastUpdateTs(name) }))
          .sort((a, b) => {
              if (b.score !== a.score) return b.score - a.score;
              return a.ts - b.ts;
          });

    rows.forEach((row, idx) => {
        const { name, score } = row;
        const tr = document.createElement('tr');
        tr.className = `${idx % 2 ? 'bg-slate-800/20' : ''} ${row.name === currentUser ? 'bg-green-900/20' : ''}`;

        tr.innerHTML = `
      <td class="px-6 py-2 text-slate-300">${idx + 1}</td>
      <td class="px-6 py-2 text-slate-200">${name}</td>
      <td class="px-6 py-4 text-right text-green-400 font-semibold">${score}</td>
      <td class="px-6 py-2 text-right text-slate-100">
        ${timelines.get(name).length ?? 0}
      </td>
    `;
        tbody.appendChild(tr);
    });
}

/* ------------------------------------------------------------------ */
/*  BOOTSTRAP                                                         */
/* ------------------------------------------------------------------ */
document.addEventListener('DOMContentLoaded', initApp);
initChart();

async function initApp() {
    try {
        /*  The session cookie (if any) goes automatically because we
            set credentials:"include".  This lightweight call returns
            200 when a valid session exists, 401 otherwise.            */
        const res = await fetch('/api/me', { credentials: 'include' });

        if (!res.ok) throw new Error('401');
        ujson = await res.json();
        const { user } = ujson;      // e.g. { "user": "alice" }
        finishLogin(ujson);                      // we’re still logged in ✔

        const hash = location.hash.slice(1);           // drop "#"
        const m = hash.match(/^challenge-(\d+)$/);
        if (m) showChallenge(Number(m[1]), false);
    } catch (e) {
        /* No valid session → show the login screen */
        console.log(e);
        console.log("No user");
        showLogin();
    }
}

async function handleNameSubmit (e) {
    e.preventDefault();
    const name = document.getElementById('display-name').value.trim();
    if (!name) return;

    try {
        const res = await fetch('/api/set-name', {          // new endpoint
            method     : 'POST',
            credentials: 'include',
            headers    : { 'Content-Type': 'application/json' },
            body       : JSON.stringify({ name })
        });
        if (!res.ok) throw new Error();

        hideNameModal();
        document.getElementById('user-name').textContent = name;
        currentUser = name;
        /* scoreboard row will pick it up automatically because `currentUser`
           is the key; name is cosmetic only */
    } catch {
        showError('Couldn’t save name. Try again.');
    }
}

window.addEventListener('popstate', e => {
    const st = e.state || { view:'challenges' };

    if      (st.view === 'challenge')   showChallenge(st.id); // no push
    else if (st.view === 'scoreboard')  showView('scoreboard');
    else                                showView('challenges');
});

/* ────────── name-modal helpers ────────── */
function showNameModal ()   { document.getElementById('name-modal').classList.remove('hidden'); }
function hideNameModal ()   { document.getElementById('name-modal').classList.add('hidden');    }
