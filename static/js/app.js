/* ------------------------------------------------------------------ */
/*  GLOBAL STATE                                                      */
/* ------------------------------------------------------------------ */

let ws = null;

let currentUser        = null;
let userPoints         = 0;
let currentView        = 'challenges';
const solvedChallenges = new Set();

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

/* ------------------------------------------------------------------ */
/*  EVENT LISTENERS                                                   */
/* ------------------------------------------------------------------ */
document.getElementById('login-form')   .addEventListener('submit', handleLogin);
document.getElementById('challenges-btn').addEventListener('click', () => showView('challenges'));
document.getElementById('scoreboard-btn').addEventListener('click', () => showView('scoreboard'));
document.getElementById('logout-btn')   .addEventListener('click', handleLogout);
document.getElementById('back-btn')     .addEventListener('click', () => showView('challenges'));


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
function finishLogin ({ username, displayname, needs_name }) {

    currentUser = displayname || username;
    window.currentUser = username;                  // keep it globally if you like
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

    showView('challenges');

    ws = new WebSocket('ws://localhost:12345/scorestream');

    ws.addEventListener('open', () => console.log('socket open'));

    ws.addEventListener('message', e => {
        const msg = JSON.parse(e.data);

        // 1. If the message is for me, record the solve
        if (msg.displayname === currentUser) {
            solvedChallenges.add(msg.challenge_id);

            // ▸ update the list or the detail panel that might be open
            if (currentView === 'challenges') {
                renderChallenges();                 // redraw grid → checkmark appears
            } else if (
                !challengeDetail.classList.contains('hidden') &&
                    openChallengeId == msg.challenge_id   // see note below
            ) {
                showChallenge(msg.challenge_id);    // redraw detail view
            }

            // ▸ keep the points read-out in the navbar current
            console.log("POINTS");
            console.log(userPoints);
            userPoints = (scoreboard.get(currentUser) || 0) + msg.points;
            console.log(userPoints);
            console.log(currentUser);
            document.getElementById('user-points').textContent = `${userPoints} pts`;
        }

        // 2. Always update the chart/scoreboard
        updateChart(msg);
    });

    ws.addEventListener('close', () => console.log('socket closed'));

}

function showLogin() {
    loginPage.classList.remove('hidden');
    banner.classList.add('hidden');
    navigation.classList.add('hidden');
    mainContent.classList.add('hidden');
    /* optional: clear form fields & focus username input */
}

async function handleLogin(e) {
    e.preventDefault();

    const username = document.getElementById('username').value.trim();
    const password = document.getElementById('password').value;   // NEW

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

        const { displayname, needs_name } = await res.json();

        finishLogin({ username, displayname, needs_name });


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
function showView(view) {
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

        categoryItems.forEach(challenge => {
            const isSolved = solvedChallenges.has(challenge.id);

            const card = document.createElement('div');
            card.className = 'bg-slate-800/50 border border-slate-700 rounded-lg p-6 hover:border-green-400 transition-colors cursor-pointer transform hover:scale-105 duration-200';
            card.innerHTML = `
        <div class="flex items-start justify-between mb-4">
          <div>
            <h3 class="text-xl font-semibold text-white mb-2">${challenge.title}</h3>
          </div>
          ${isSolved ? '<div class="text-green-400"><svg class="w-6 h-6" fill="currentColor" viewBox="0 0 20 20"><path fill-rule="evenodd" d="M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z" clip-rule="evenodd"></path></svg></div>' : ''}
        </div>
        <p class="text-slate-300 text-sm mb-4">${challenge.description}</p>
        <div class="flex items-center justify-between">
          <span class="text-${getDifficultyColor(challenge.difficulty)}-400 text-sm font-medium">
            ${challenge.difficulty}
          </span>
          <span class="text-green-400 font-semibold">${challenge.points} pts</span>
        </div>
      `;

            card.addEventListener('click', () => showChallenge(challenge.id));
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

function showChallenge(challengeId) {
    openChallengeId = challengeId;
    const challenge = challenges.find(c => c.id === challengeId);
    if (!challenge) return;

    const isSolved = solvedChallenges.has(challengeId);

    challengeList .classList.add   ('hidden');
    challengeDetail.classList.remove('hidden');

    document.getElementById('challenge-content').innerHTML = generateChallengeHTML(challenge, isSolved);

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
            solvedChallenges.add(challengeId);
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
        <p class="text-slate-300">${ch.description}</p>
      </div>

      <div class="mb-8">
        <h2 class="text-xl font-semibold text-white mb-3">Challenge</h2>
        <div class="bg-slate-900/50 border border-slate-600 rounded-lg p-4">
          <pre class="text-slate-300 text-sm whitespace-pre-wrap">${ch.content}</pre>
        </div>
      </div>

      ${!isSolved ? `
        <div class="border-t border-slate-700 pt-6">
        <h2 class="text-xl font-semibold text-white mb-4">Submit Flag</h2>
        <form id="flag-form-${ch.id}" class="flex gap-3">
        <input type="text" id="flag-input-${ch.id}" placeholder="CTF{...}"
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
    const rows = [...scoreboard.entries()].map(([team, score]) => ({
        team,
        score,
        solved : (timelines.get(team)?.length || 0)          // events – initial point
    }));

    /* 2 ─ make sure the current player shows up even with 0 pts */
    if (currentUser && !scoreboard.has(currentUser)) {
        rows.push({ team: currentUser, score: userPoints, solved: solvedChallenges.size });
    }

    /* 3 ─ rank & render */
    rows.sort((a, b) => b.score - a.score)
        .forEach((e, i) => {
            const tr = document.createElement('tr');
            tr.className = `border-b border-slate-700 ${e.team === currentUser ? 'bg-green-900/20' : ''}`;
            tr.innerHTML = `
          <td class="px-6 py-4 text-slate-300">#${i + 1}</td>
          <td class="px-6 py-4 text-white font-medium">${e.team}</td>
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

        challenges = Object.values(challengesByCategory).flat();

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
let scoreChart;                       // Chart.js instance

function initChart () {
    const ctx = document.getElementById('score-chart').getContext('2d');
    scoreChart = new Chart(ctx, {
        type: 'line',
        data: { datasets: [] },
        options: {
            animation: false,
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

function updateChart (msg) {
    /* ---------- 1. unpack & validate -------------------------------- */
    const timestamp = msg.ts;          // epoch ms
    const name      = msg.displayname;
    const delta     = msg.points;      // this message’s *increment*

    if (timestamp == null || name == null || delta == null) {
        console.warn('Malformed score message', msg);
        return;
    }

    /* ---------- 2. update running total ----------------------------- */
    const prevTotal   = scoreboard.get(name) || 0;
    const newTotal    = prevTotal + delta;         // accumulate
    scoreboard.set(name, newTotal);

    /* ---------- 3. append to the player’s timeline ------------------ */
    let tl = timelines.get(name);
    if (!tl) {
        tl = [];
        timelines.set(name, tl);
    }
    tl.push({ x: timestamp, y: newTotal, challenge: msg.challenge });

    /* ---------- 4. rebuild top-N datasets --------------------------- */
    const topUsers = [...scoreboard.entries()]
          .sort((a, b) => b[1] - a[1])   // highest score first
          .slice(0, TOP_N)
          .map(([u]) => u);

    scoreChart.data.datasets = topUsers.map((u, idx) => {
        const existing = scoreChart.data.datasets.find(d => d.label === u);
        const color    = existing ? existing.borderColor
              : palette[idx % palette.length];
        return {
            label          : u,
            data           : timelines.get(u),
            borderColor    : color,
            backgroundColor: color,
            pointRadius    : 3,
            tension        : 0.3,
            borderWidth    : 2
        };
    });

    scoreChart.update('none');      // instant, no animation
    refreshScoreboard(topUsers);
}

function refreshScoreboard (topUsers) {
    const tbody = document.getElementById('scoreboard-body');
    tbody.innerHTML = '';                       // clear

    topUsers.forEach((name, idx) => {
        const tr = document.createElement('tr');
        tr.className = idx % 2 ? 'bg-slate-800/20' : '';   // zebra

        tr.innerHTML = `
      <td class="px-6 py-2 text-slate-300">${idx + 1}</td>
      <td class="px-6 py-2 text-slate-200">${name}</td>
      <td class="px-6 py-4 text-right text-green-400 font-semibold">${scoreboard.get(name)}</td>
      <td class="px-6 py-2 text-right text-slate-100">
        ${timelines.get(name).length}
      </td>
    `;
        tbody.appendChild(tr);
    });
}

/* ------------------------------------------------------------------ */
/*  BOOTSTRAP                                                         */
/* ------------------------------------------------------------------ */
document.addEventListener('DOMContentLoaded', initApp);
/* document.addEventListener('DOMContentLoaded', initChart);*/
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

/* ────────── name-modal helpers ────────── */
function showNameModal ()   { document.getElementById('name-modal').classList.remove('hidden'); }
function hideNameModal ()   { document.getElementById('name-modal').classList.add('hidden');    }
