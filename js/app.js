/**
 * CTFG Frontend Application
 * 
 * A secure, real-time CTF (Capture The Flag) game interface with:
 * - XSS-protected challenge rendering
 * - Rate-limited API interactions
 * - Resilient WebSocket connections
 * - Input validation and sanitization
 * - Progressive error handling
 * 
 * @version 0.2.0
 * @author CTFG Team
 */

'use strict';

/* =================================================================== */
/* CONFIGURATION & CONSTANTS                                           */
/* =================================================================== */

/**
 * WebSocket reconnection configuration
 */
const WEBSOCKET_CONFIG = {
    BACKOFF_MIN: 1_000,               // 1 second minimum delay
    BACKOFF_MAX: 30_000,              // 30 seconds maximum delay
    MAX_RECONNECT_ATTEMPTS: 10,       // Maximum reconnection attempts
    CONNECTION_TIMEOUT: 10_000,       // 10 seconds connection timeout
    PING_INTERVAL: 45_000,            // 45 seconds ping interval
    MAX_MESSAGE_SIZE: 5_000_000       // 5MB maximum message size
};

/**
 * UI and validation constants
 */
const UI_CONFIG = {
    ERROR_DISPLAY_TIME: 5000,         // 5 seconds error display
    SUCCESS_DISPLAY_TIME: 1500,       // 1.5 seconds success display
    MAX_INPUT_LENGTH: 1000,           // Default input length limit
    MAX_USERNAME_LENGTH: 50,          // Username length limit
    MAX_PASSWORD_LENGTH: 100,         // Password length limit
    MAX_DESCRIPTION_LENGTH: 10000,    // Challenge description limit
    MIN_PASSWORD_LENGTH: 3            // Minimum password length
};

/**
 * Scoreboard configuration
 */
const SCOREBOARD_CONFIG = {
    TOP_N: 10,                        // Show top 10 players
    CHART_COLORS: [                   // Color palette for charts
        '#60a5fa', '#f87171', '#34d399', '#e879f9', '#fbbf24',
        '#a78bfa', '#fb923c', '#4ade80', '#f472b6', '#38bdf8'
    ]
};

/* =================================================================== */
/* GLOBAL STATE MANAGEMENT                                             */
/* =================================================================== */

/**
 * Application state container
 */
const AppState = {
    // User session
    currentUser: null,
    userPoints: 0,
    
    // UI state
    currentView: 'challenges',
    
    // Challenge state
    solvedChallenges: new Set(),
    revealedHints: new Set(),
    challengesByCategory: {},
    challenges: [],
    
    // WebSocket state
    ws: null,
    reconnectDelay: WEBSOCKET_CONFIG.BACKOFF_MIN,
    reconnectAttempts: 0,
    pingTimer: null,
    
    // Scoreboard state
    timelines: new Map(),             // username → [{x, y}, …]
    scoreboard: new Map(),            // username → latest points
    seenEventIDs: new Set(),
    scoreChart: null
};

/**
 * DOM element cache for performance
 */
const DOMElements = {
    loginPage: document.getElementById('login-page'),
    banner: document.getElementById('banner'),
    navigation: document.getElementById('navigation'),
    mainContent: document.getElementById('main-content'),
    challengeList: document.getElementById('challenge-list'),
    challengeDetail: document.getElementById('challenge-detail'),
    scoreboardElm: document.getElementById('scoreboard')
};

/* =================================================================== */
/* UTILITY FUNCTIONS                                                   */
/* =================================================================== */

/**
 * Convert value to integer safely
 * @param {any} x - Value to convert
 * @returns {number} Integer value
 */
const asID = x => Number(x);

/**
 * Get current timestamp in microseconds
 * @returns {number} Current timestamp in microseconds
 */
const nowMicros = () => Date.now() * 1000;

/* =================================================================== */
/* SECURITY & VALIDATION UTILITIES                                     */
/* =================================================================== */

/**
 * Security utilities for input validation and XSS prevention
 */
const SecurityUtils = {
    /**
     * Escape HTML special characters to prevent XSS attacks
     * @param {string} text - The text to escape
     * @returns {string} HTML-escaped text
     */
    escapeHtml(text) {
        if (typeof text !== 'string') return text;
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    },

    /**
     * Sanitize and validate input strings
     * @param {string} input - The input to sanitize
     * @param {number} maxLength - Maximum allowed length
     * @returns {string} Sanitized input
     */
    sanitizeInput(input, maxLength = UI_CONFIG.MAX_INPUT_LENGTH) {
        if (typeof input !== 'string') return '';
        return input.trim().slice(0, maxLength);
    },

    /**
     * Validate username format (alphanumeric + underscore/hyphen)
     * @param {string} username - Username to validate
     * @returns {boolean} Whether username is valid
     */
    isValidUsername(username) {
        return /^[a-zA-Z0-9_-]{3,20}$/.test(username);
    },

    /**
     * Validate WebSocket URL format
     * @param {string} url - WebSocket URL to validate
     * @returns {boolean} Whether URL is valid
     */
    isValidWebSocketUrl(url) {
        return url.startsWith('ws://') || url.startsWith('wss://');
    },

    /**
     * Create safe error messages that don't leak sensitive information
     * @param {string} errorType - Type of error
     * @param {string} fallback - Fallback message
     * @returns {string} Safe error message
     */
    createSafeErrorMessage(errorType, fallback = 'An error occurred') {
        const safeErrors = {
            'network': 'Connection error. Please try again.',
            'validation': 'Invalid input. Please check your data.',
            'auth': 'Authentication failed. Please log in again.',
            'permission': 'You do not have permission to perform this action.',
            'rate_limit': 'Too many requests. Please wait and try again.',
            'server': 'Server error. Please try again later.'
        };
        return safeErrors[errorType] || fallback;
    }
};

/* =================================================================== */
/* ERROR HANDLING & USER FEEDBACK                                      */
/* =================================================================== */

/**
 * User interface utilities for notifications and feedback
 */
const UIUtils = {
    /**
     * Show a red notification banner for errors
     * @param {string} message - Error message to display
     * @param {number} [ms=5000] - Display duration in milliseconds (0 = permanent)
     */
    showError(message, ms = UI_CONFIG.ERROR_DISPLAY_TIME) {
        // Ensure we have a host container
        let host = document.getElementById('error-host');
        if (!host) {
            host = document.createElement('div');
            host.id = 'error-host';
            host.className = 'fixed top-4 right-4 space-y-2 z-50';
            document.body.appendChild(host);
        }

        // Build error card with escaped content
        const card = document.createElement('div');
        card.className = 'bg-red-600 text-white px-4 py-3 rounded shadow-lg flex items-start gap-3 animate-[slide-in_.2s_ease-out]';
        card.innerHTML = `
            <span class="flex-1 text-sm">${SecurityUtils.escapeHtml(message)}</span>
            <button class="font-bold ml-3 hover:text-red-200 focus:outline-none" aria-label="Close">&times;</button>
        `;

        // Close button behavior
        card.querySelector('button').addEventListener('click', () => card.remove());
        host.appendChild(card);

        // Auto-dismiss if requested
        if (ms > 0) {
            setTimeout(() => card.remove(), ms);
        }
    },

    /**
     * Generate success HTML with escaped content
     * @param {string|number} points - Points awarded
     * @returns {string} Success HTML
     */
    generateSuccessHtml(points) {
        const safePoints = SecurityUtils.escapeHtml(String(points));
        return `
            <div class="flex items-center gap-2 text-green-400">
                <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 20 20">
                    <path fill-rule="evenodd"
                        d="M16.707 5.293a1 1 0 011.414 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z"
                        clip-rule="evenodd"></path>
                </svg>
                <span class="font-semibold">Correct! +${safePoints} points</span>
            </div>
        `;
    },

    /**
     * Generate error HTML with escaped content
     * @param {string} msg - Error message
     * @returns {string} Error HTML
     */
    generateErrorHtml(msg) {
        const safeMsg = SecurityUtils.escapeHtml(msg);
        return `
            <div class="flex items-center gap-2 text-red-400">
                <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 20 20">
                    <path fill-rule="evenodd"
                        d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                        clip-rule="evenodd"></path>
                </svg>
                <span>${safeMsg}</span>
            </div>
        `;
    }
};

/* =================================================================== */
/* AUTHENTICATION & SESSION MANAGEMENT                                 */
/* =================================================================== */

/**
 * Authentication and session management utilities
 */
const AuthManager = {
    /**
     * Handle user login form submission
     * @param {Event} e - Form submission event
     */
    async handleLogin(e) {
        e.preventDefault();

        // Sanitize and validate inputs
        const username = SecurityUtils.sanitizeInput(
            document.getElementById('username').value, 
            UI_CONFIG.MAX_USERNAME_LENGTH
        );
        const password = SecurityUtils.sanitizeInput(
            document.getElementById('password').value, 
            UI_CONFIG.MAX_PASSWORD_LENGTH
        );

        // Client-side validation
        if (!username || !password) {
            UIUtils.showError(SecurityUtils.createSafeErrorMessage('validation', 'Username and password are required'));
            return;
        }

        if (!SecurityUtils.isValidUsername(username)) {
            UIUtils.showError(SecurityUtils.createSafeErrorMessage('validation', 'Username must be 3-20 characters (letters, numbers, _, -)'));
            return;
        }

        if (password.length < UI_CONFIG.MIN_PASSWORD_LENGTH) {
            UIUtils.showError(SecurityUtils.createSafeErrorMessage('validation', 'Password too short'));
            return;
        }

        try {
            // Call the server
            const res = await fetch('/api/login', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ username, password })
            });

            // Handle various response codes
            if (res.status === 401) {
                UIUtils.showError(SecurityUtils.createSafeErrorMessage('auth', 'Invalid credentials'));
                return;
            }
            if (res.status === 429) {
                UIUtils.showError(SecurityUtils.createSafeErrorMessage('rate_limit'));
                return;
            }
            if (!res.ok) {
                UIUtils.showError(SecurityUtils.createSafeErrorMessage('server'));
                return;
            }

            const data = await res.json();
            const { displayname, needs_name, websocket_url } = data;

            this.finishLogin({ 
                username: SecurityUtils.escapeHtml(username), 
                displayname: SecurityUtils.escapeHtml(displayname || ''), 
                needs_name, 
                websocket_url: SecurityUtils.escapeHtml(websocket_url || '') 
            });

        } catch (err) {
            console.error('Login failed:', err);
            UIUtils.showError(SecurityUtils.createSafeErrorMessage('network'));
        }
    },

    /**
     * Complete the login process and initialize the application
     * @param {Object} userData - User data from login response
     */
    finishLogin({ username, displayname, needs_name, websocket_url }) {
        // Update application state
        AppState.currentUser = displayname || username;
        window.currentUser = username;
        
        // Update UI
        document.getElementById('user-name').textContent = displayname || '(unnamed)';
        document.getElementById('user-points').textContent = '0 pts';
        
        // Show main interface
        this.showMainInterface();
        
        // Handle first-login onboarding
        if (needs_name) {
            this.showNameModal();
        }
        
        // Load initial data and connect WebSocket
        ChallengeManager.loadChallenges();
        ViewManager.showView('challenges');
        WebSocketManager.connect(websocket_url);
    },

    /**
     * Show the main application interface
     */
    showMainInterface() {
        DOMElements.loginPage.classList.add('hidden');
        DOMElements.banner.classList.remove('hidden');
        DOMElements.navigation.classList.remove('hidden');
        DOMElements.mainContent.classList.remove('hidden');
    },

    /**
     * Show the login interface
     */
    showLogin() {
        DOMElements.loginPage.classList.remove('hidden');
        DOMElements.banner.classList.add('hidden');
        DOMElements.navigation.classList.add('hidden');
        DOMElements.mainContent.classList.add('hidden');
        
        // Hide any modals that might be open
        this.hideNameModal();
    },

    /**
     * Handle user logout
     */
    async handleLogout() {
        try {
            await fetch('/api/logout', {
                method: 'POST',
                credentials: 'include'
            });
        } catch (err) {
            console.error('Logout error:', err);
        } finally {
            // Clean up state
            AppState.currentUser = null;
            WebSocketManager.disconnect();
            this.showLogin();
        }
    },

    /**
     * Show the name setting modal for new users
     */
    showNameModal() {
        const nameModal = document.getElementById('name-modal');
        if (nameModal) {
            nameModal.classList.remove('hidden');
        }
        const nameForm = document.getElementById('name-form');
        if (nameForm) {
            nameForm.addEventListener('submit', this.handleNameSubmit, { once: true });
        }
    },

    /**
     * Hide the name setting modal
     */
    hideNameModal() {
        const nameModal = document.getElementById('name-modal');
        if (nameModal) {
            nameModal.classList.add('hidden');
        }
    },

    /**
     * Handle display name submission
     * @param {Event} e - Form submission event
     */
    async handleNameSubmit(e) {
        e.preventDefault();
        const name = SecurityUtils.sanitizeInput(
            document.getElementById('display-name')?.value?.trim() || ''
        );
        if (!name) return;

        try {
            const res = await fetch('/api/set-name', {
                method: 'POST',
                credentials: 'include',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ name })
            });
            
            if (!res.ok) throw new Error(`HTTP ${res.status}`);

            AuthManager.hideNameModal();
            document.getElementById('user-name').textContent = name;
            AppState.currentUser = name;
            
        } catch (err) {
            console.error('Name submission error:', err);
            UIUtils.showError(SecurityUtils.createSafeErrorMessage('network', 'Could not save name'));
        }
    }
};

/* =================================================================== */
/* WEBSOCKET MANAGEMENT                                                */
/* =================================================================== */

/**
 * WebSocket connection and real-time communication management
 */
const WebSocketManager = {
    /**
     * Connect to WebSocket server with validation and error handling
     * @param {string} websocket_url - WebSocket server URL
     */
    connect(websocket_url) {
        if (!AppState.currentUser) return;
        
        // Check reconnection limits
        if (AppState.reconnectAttempts >= WEBSOCKET_CONFIG.MAX_RECONNECT_ATTEMPTS) {
            console.warn('Maximum WebSocket reconnection attempts reached');
            UIUtils.showError(SecurityUtils.createSafeErrorMessage('network', 'Lost connection to server. Please refresh the page.'));
            return;
        }

        // Validate and sanitize WebSocket URL
        const sanitizedUrl = SecurityUtils.sanitizeInput(websocket_url, 200);
        if (!SecurityUtils.isValidWebSocketUrl(sanitizedUrl)) {
            console.error('Invalid WebSocket URL:', sanitizedUrl);
            return;
        }
        
        console.log(`WebSocket connection attempt ${AppState.reconnectAttempts + 1}/${WEBSOCKET_CONFIG.MAX_RECONNECT_ATTEMPTS}`);
        
        AppState.ws = new WebSocket(sanitizedUrl);
        
        // Connection timeout
        const connectionTimeout = setTimeout(() => {
            if (AppState.ws.readyState === WebSocket.CONNECTING) {
                AppState.ws.close();
                console.warn('WebSocket connection timeout');
            }
        }, WEBSOCKET_CONFIG.CONNECTION_TIMEOUT);
        
        this.setupEventListeners(websocket_url, connectionTimeout);
        this.startPingInterval();
    },

    /**
     * Set up WebSocket event listeners
     * @param {string} websocket_url - Original WebSocket URL for reconnection
     * @param {number} connectionTimeout - Connection timeout ID
     */
    setupEventListeners(websocket_url, connectionTimeout) {
        AppState.ws.addEventListener('open', () => {
            console.log('WebSocket connected');
            clearTimeout(connectionTimeout);
            AppState.reconnectDelay = WEBSOCKET_CONFIG.BACKOFF_MIN;
            AppState.reconnectAttempts = 0;
        });

        AppState.ws.addEventListener('message', (e) => {
            try {
                // Validate message size
                if (e.data.length > WEBSOCKET_CONFIG.MAX_MESSAGE_SIZE) {
                    console.warn('Received oversized WebSocket message, ignoring');
                    return;
                }
                
                let payload = JSON.parse(e.data);
                
                if (Array.isArray(payload)) {
                    // Batch update - process all events
                    payload.forEach(msg => ScoreboardManager.handleScoreEvent(msg, true));
                    ScoreboardManager.flushChart();
                    // Update user points after processing all historical events
                    ScoreboardManager.updateUserPoints();
                } else {
                    this.dispatchEvent(payload);
                }
            } catch (error) {
                console.error('Error processing WebSocket message:', error);
            }
        });

        AppState.ws.addEventListener('close', (e) => {
            console.log('WebSocket closed', { code: e.code, reason: e.reason, wasClean: e.wasClean });
            clearInterval(AppState.pingTimer);
            clearTimeout(connectionTimeout);
            
            this.handleReconnection(websocket_url);
        });

        AppState.ws.addEventListener('error', (err) => {
            console.error('WebSocket error', err);
            clearTimeout(connectionTimeout);
            AppState.ws.close();
        });
    },

    /**
     * Handle WebSocket reconnection with exponential backoff
     * @param {string} websocket_url - WebSocket URL to reconnect to
     */
    handleReconnection(websocket_url) {
        AppState.reconnectAttempts++;
        
        if (AppState.reconnectAttempts < WEBSOCKET_CONFIG.MAX_RECONNECT_ATTEMPTS) {
            const delay = AppState.reconnectDelay + Math.random() * 1000;
            console.log(`Reconnecting in ${delay}ms (attempt ${AppState.reconnectAttempts})`);
            
            setTimeout(() => this.connect(websocket_url), delay);
            AppState.reconnectDelay = Math.min(AppState.reconnectDelay * 2, WEBSOCKET_CONFIG.BACKOFF_MAX);
        } else {
            UIUtils.showError(SecurityUtils.createSafeErrorMessage('network', 'Connection lost. Please refresh the page.'));
        }
    },

    /**
     * Start ping interval to keep connection alive
     */
    startPingInterval() {
        clearInterval(AppState.pingTimer);
        AppState.pingTimer = setInterval(() => {
            if (AppState.ws && AppState.ws.readyState === WebSocket.OPEN) {
                AppState.ws.send(JSON.stringify({ type: 'ping' }));
            }
        }, WEBSOCKET_CONFIG.PING_INTERVAL);
    },

    /**
     * Dispatch incoming WebSocket events
     * @param {Object} payload - Event payload
     */
    dispatchEvent(payload) {
        switch (payload.type) {
            case 'score':
            case 'hint':
                ScoreboardManager.handleScoreEvent(payload);
                break;
            default:
                ScoreboardManager.handleScoreEvent(payload);
        }
    },

    /**
     * Disconnect WebSocket and clean up
     */
    disconnect() {
        if (AppState.ws) {
            AppState.ws.close();
            AppState.ws = null;
        }
        clearInterval(AppState.pingTimer);
        AppState.reconnectAttempts = 0;
        AppState.reconnectDelay = WEBSOCKET_CONFIG.BACKOFF_MIN;
    }
};

/* =================================================================== */
/* VIEW MANAGEMENT                                                     */
/* =================================================================== */

/**
 * UI view management and navigation
 */
const ViewManager = {
    /**
     * Show a specific view (challenges or scoreboard)
     * @param {string} view - View name ('challenges' or 'scoreboard')
     * @param {boolean} push - Whether to push to browser history
     */
    showView(view, push = false) {
        AppState.currentView = view;
        
        if (push) {
            history.pushState({ view }, '', `#${view}`);
        }
        
        switch (view) {
            case 'challenges':
                this.showChallengesView();
                break;
            case 'scoreboard':
                this.showScoreboardView();
                break;
            default:
                console.warn('Unknown view:', view);
        }
    },

    /**
     * Show the challenges view
     */
    showChallengesView() {
        DOMElements.challengeList.classList.remove('hidden');
        DOMElements.challengeDetail.classList.add('hidden');
        DOMElements.scoreboardElm.classList.add('hidden');
        
        ChallengeRenderer.renderChallengeGrid();
    },

    /**
     * Show the scoreboard view
     */
    showScoreboardView() {
        DOMElements.challengeList.classList.add('hidden');
        DOMElements.challengeDetail.classList.add('hidden');
        DOMElements.scoreboardElm.classList.remove('hidden');
        
        ScoreboardManager.refreshScoreboard();
        ScoreboardManager.updateChart();
    },

    /**
     * Show individual challenge detail view
     * @param {number} challengeId - Challenge ID to show
     * @param {boolean} push - Whether to push to browser history
     */
    showChallenge(challengeId, push = false) {
        const challenge = AppState.challenges.find(c => asID(c.id) === asID(challengeId));
        if (!challenge) {
            console.error('Challenge not found:', challengeId);
            return;
        }

        if (push) {
            history.pushState({ view: 'challenge', challengeId }, '', `#challenge-${challengeId}`);
        }

        const isSolved = AppState.solvedChallenges.has(asID(challengeId));
        
        DOMElements.challengeList.classList.add('hidden');
        DOMElements.challengeDetail.classList.remove('hidden');
        
        document.getElementById('challenge-content').innerHTML = 
            ChallengeRenderer.generateChallengeHTML(challenge, isSolved);
        
        // Set up event listeners for the challenge
        this.setupChallengeEventListeners(challengeId, isSolved);
    },

    /**
     * Set up event listeners for challenge interactions
     * @param {number} challengeId - Challenge ID
     * @param {boolean} isSolved - Whether challenge is already solved
     */
    setupChallengeEventListeners(challengeId, isSolved) {
        // Hint reveal buttons
        document.querySelectorAll('.reveal-hint-btn').forEach(btn =>
            btn.addEventListener('click', ChallengeManager.handleRevealHint.bind(ChallengeManager)));

        // Flag submission form (only if not solved)
        if (!isSolved) {
            const flagForm = document.getElementById(`flag-form-${challengeId}`);
            if (flagForm) {
                flagForm.addEventListener('submit', e => ChallengeManager.handleFlagSubmit(e, challengeId));
            }
        }
    }
};

/* =================================================================== */
/* CHALLENGE MANAGEMENT                                                */
/* =================================================================== */

/**
 * Challenge data management and API interactions
 */
const ChallengeManager = {
    /**
     * Load challenges from the server
     */
    async loadChallenges() {
        try {
            const res = await fetch('/api/challenges', { credentials: 'include' });
            
            if (res.status === 401) {
                UIUtils.showError(SecurityUtils.createSafeErrorMessage('auth'));
                AuthManager.showLogin();
                return;
            }
            
            if (!res.ok) {
                throw new Error(`HTTP ${res.status}`);
            }

            const data = await res.json();
            this.processChallengeData(data);
            
            if (AppState.currentView === 'challenges') {
                ChallengeRenderer.renderChallengeGrid();
            }
            
        } catch (err) {
            console.error('Failed to load challenges:', err);
            UIUtils.showError(SecurityUtils.createSafeErrorMessage('network', 'Could not load challenges'));
        }
    },

    /**
     * Process and organize challenge data
     * @param {Array|Object} data - Challenge data from server
     */
    processChallengeData(data) {
        // Handle both array and object formats
        if (Array.isArray(data)) {
            AppState.challengesByCategory = data.reduce((acc, c) => {
                (acc[c.category] ||= []).push(c);
                return acc;
            }, {});
        } else {
            AppState.challengesByCategory = data;
        }

        // Sort challenges by ID within each category
        Object.values(AppState.challengesByCategory).forEach(list =>
            list.sort((a, b) => Number(a.id) - Number(b.id)));

        AppState.challenges = Object.values(AppState.challengesByCategory).flat();

        // Update solved challenges based on server data
        AppState.solvedChallenges.clear();
        AppState.challenges.forEach(ch => {
            if (ch.solved) {
                AppState.solvedChallenges.add(asID(ch.id));
            }
        });

        // Update revealed hints based on server data
        this.updateRevealedHints();
    },

    /**
     * Update revealed hints based on server data
     */
    updateRevealedHints() {
        AppState.revealedHints.clear();
        AppState.challenges.forEach(ch =>
            ch.hints?.forEach(h => {
                if (h.text) {
                    AppState.revealedHints.add(`${ch.id}:${h.id}`);
                }
            }));
    },

    /**
     * Handle flag submission
     * @param {Event} e - Form submission event
     * @param {number} challengeId - Challenge ID
     */
    async handleFlagSubmit(e, challengeId) {
        e.preventDefault();

        const flagInput = document.getElementById(`flag-input-${challengeId}`);
        const resultDiv = document.getElementById(`flag-result-${challengeId}`);
        const submitted = SecurityUtils.sanitizeInput(flagInput.value);

        if (!submitted) return;

        try {
            const res = await fetch('/api/submit', {
                method: 'POST',
                credentials: 'include',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ id: challengeId, flag: submitted })
            });

            await this.handleSubmissionResponse(res, challengeId, resultDiv);

        } catch (err) {
            console.error('Flag submission error:', err);
            UIUtils.showError(SecurityUtils.createSafeErrorMessage('network'));
        } finally {
            flagInput.value = '';
        }
    },

    /**
     * Handle flag submission response
     * @param {Response} res - Fetch response
     * @param {number} challengeId - Challenge ID
     * @param {HTMLElement} resultDiv - Result display element
     */
    async handleSubmissionResponse(res, challengeId, resultDiv) {
        if (res.status === 401) {
            UIUtils.showError(SecurityUtils.createSafeErrorMessage('auth', 'Please log in again'));
            AuthManager.showLogin();
            return;
        }
        
        if (res.status === 429) {
            UIUtils.showError(SecurityUtils.createSafeErrorMessage('rate_limit'));
            return;
        }
        
        if (!res.ok) {
            UIUtils.showError(SecurityUtils.createSafeErrorMessage('server'));
            return;
        }

        const data = await res.json();
        
        if (data.result === 'correct') {
            AppState.solvedChallenges.add(asID(challengeId));
            await this.loadChallenges();
            ChallengeRenderer.renderChallengeGrid();
            resultDiv.innerHTML = UIUtils.generateSuccessHtml(data.points || 0);
            // Show solved view immediately
            ViewManager.showChallenge(challengeId);
        } else if (data.result === 'already_solved') {
            resultDiv.innerHTML = `<div class="text-green-400">Already solved!</div>`;
        } else {
            resultDiv.innerHTML = UIUtils.generateErrorHtml('Incorrect flag. Try again!');
        }
    },

    /**
     * Handle hint reveal button click
     * @param {Event} e - Click event
     */
    async handleRevealHint(e) {
        const btn = e.currentTarget;
        const chID = Number(btn.dataset.ch);
        const hintID = Number(btn.dataset.hint);

        btn.disabled = true;
        btn.textContent = '…';

        try {
            const res = await fetch('/api/hint', {
                method: 'POST',
                credentials: 'include',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ id: chID, hint_id: hintID })
            });

            await this.handleHintResponse(res, chID, btn);

        } catch (err) {
            console.error('Hint reveal error:', err);
            UIUtils.showError(SecurityUtils.createSafeErrorMessage('network'));
            this.resetHintButton(btn);
        }
    },

    /**
     * Handle hint reveal response
     * @param {Response} res - Fetch response
     * @param {number} chID - Challenge ID
     * @param {HTMLElement} btn - Hint button element
     */
    async handleHintResponse(res, chID, btn) {
        if (res.status === 401) {
            UIUtils.showError(SecurityUtils.createSafeErrorMessage('auth'));
            AuthManager.showLogin();
            return;
        }
        
        if (res.status === 429) {
            UIUtils.showError(SecurityUtils.createSafeErrorMessage('rate_limit'));
            this.resetHintButton(btn);
            return;
        }
        
        if (!res.ok) {
            UIUtils.showError(SecurityUtils.createSafeErrorMessage('server'));
            this.resetHintButton(btn);
            return;
        }

        const data = await res.json();
        
        if (data.result === 'ok') {
            await this.loadChallenges();
            ViewManager.showChallenge(chID);
        } else {
            const errorType = data.message?.includes('points') ? 'permission' : 'server';
            UIUtils.showError(SecurityUtils.createSafeErrorMessage(errorType));
            this.resetHintButton(btn);
        }
    },

    /**
     * Reset hint button to original state
     * @param {HTMLElement} btn - Button element
     */
    resetHintButton(btn) {
        btn.disabled = false;
        btn.textContent = 'Reveal';
    }
};

/* =================================================================== */
/* CHALLENGE RENDERING                                                 */
/* =================================================================== */

/**
 * Challenge UI rendering and display utilities
 */
const ChallengeRenderer = {
    /**
     * Render the main challenge grid
     */
    renderChallengeGrid() {
        const container = document.getElementById('challenges-grid');
        if (!container) return;

        container.innerHTML = '';

        if (Object.keys(AppState.challengesByCategory).length === 0) {
            container.innerHTML = '<p class="text-slate-400">No challenges loaded. Please try again later.</p>';
            return;
        }

        Object.entries(AppState.challengesByCategory).forEach(([categoryName, categoryItems]) => {
            this.renderCategorySection(container, categoryName, categoryItems);
        });
    },

    /**
     * Render a category section with its challenges
     * @param {HTMLElement} container - Container element
     * @param {string} categoryName - Category name
     * @param {Array} categoryItems - Challenges in this category
     */
    renderCategorySection(container, categoryName, categoryItems) {
        const categorySection = document.createElement('div');
        categorySection.className = 'col-span-full mb-8';

        const categoryHeader = this.createCategoryHeader(categoryName, categoryItems.length);
        const categoryGrid = this.createCategoryGrid(categoryItems);

        categorySection.appendChild(categoryHeader);
        categorySection.appendChild(categoryGrid);
        container.appendChild(categorySection);
    },

    /**
     * Create category header element
     * @param {string} categoryName - Category name
     * @param {number} itemCount - Number of challenges in category
     * @returns {HTMLElement} Category header element
     */
    createCategoryHeader(categoryName, itemCount) {
        const categoryHeader = document.createElement('div');
        categoryHeader.className = 'flex items-center gap-3 mb-6';
        
        const safeCategoryName = SecurityUtils.escapeHtml(categoryName);
        const challengeText = itemCount !== 1 ? 'challenges' : 'challenge';
        
        categoryHeader.innerHTML = `
            <div class="w-8 h-8 ${this.getCategoryIconBg(categoryName)} rounded-lg flex items-center justify-center">
                ${this.getCategoryIcon(categoryName)}
            </div>
            <h2 class="text-2xl font-bold text-white">${safeCategoryName}</h2>
            <div class="flex-1 h-px bg-gradient-to-r from-slate-600 to-transparent"></div>
            <span class="text-slate-400 text-sm">
                ${itemCount} ${challengeText}
            </span>
        `;
        
        return categoryHeader;
    },

    /**
     * Create category grid with challenge cards
     * @param {Array} categoryItems - Challenges in this category
     * @returns {HTMLElement} Category grid element
     */
    createCategoryGrid(categoryItems) {
        const categoryGrid = document.createElement('div');
        categoryGrid.className = 'grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6';

        categoryItems.forEach(challenge => {
            const card = this.createChallengeCard(challenge);
            categoryGrid.appendChild(card);
        });

        return categoryGrid;
    },

    /**
     * Create individual challenge card
     * @param {Object} challenge - Challenge data
     * @returns {HTMLElement} Challenge card element
     */
    createChallengeCard(challenge) {
        const isSolved = AppState.solvedChallenges.has(asID(challenge.id));
        
        const baseClasses = 'rounded-lg p-6 transition-colors cursor-pointer transform hover:scale-105 duration-200';
        const solvedClasses = 'bg-green-800/40 border-green-500 hover:border-green-400';
        const unsolvedClasses = 'bg-slate-800/50 border-slate-700 hover:border-green-400';
        
        const card = document.createElement('div');
        card.className = `${baseClasses} ${isSolved ? solvedClasses : unsolvedClasses}`;

        // Use escaped content for all user-controlled data
        const safeTitle = SecurityUtils.escapeHtml(challenge.title || '');
        const safeDifficulty = SecurityUtils.escapeHtml(challenge.difficulty || '');
        const safePoints = SecurityUtils.escapeHtml(String(challenge.points || 0));
        
        const solvedIcon = isSolved ? 
            '<div class="text-green-400"><svg class="w-6 h-6" fill="currentColor" viewBox="0 0 20 20"><path fill-rule="evenodd" d="M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z" clip-rule="evenodd"></path></svg></div>' : 
            '';

        card.innerHTML = `
            <div class="flex items-start justify-between mb-4">
                <div>
                    <h3 class="text-xl font-semibold text-white mb-2">${safeTitle}</h3>
                </div>
                ${solvedIcon}
            </div>
            <div class="flex items-center justify-between">
                <span class="text-${this.getDifficultyColor(challenge.difficulty)}-400 text-sm font-medium">
                    ${safeDifficulty}
                </span>
                <span class="text-green-400 font-semibold">${safePoints} pts</span>
            </div>
        `;

        card.addEventListener('click', () => ViewManager.showChallenge(challenge.id, true));
        return card;
    },

    /**
     * Generate HTML for individual challenge detail view
     * @param {Object} ch - Challenge data
     * @param {boolean} isSolved - Whether challenge is solved
     * @returns {string} Challenge HTML
     */
    generateChallengeHTML(ch, isSolved) {
        // Safely escape all user-controlled content
        const safeTitle = SecurityUtils.escapeHtml(ch.title || '');
        const safeCategory = SecurityUtils.escapeHtml(ch.category || '');
        const safeDifficulty = SecurityUtils.escapeHtml(ch.difficulty || '');
        const safePoints = SecurityUtils.escapeHtml(String(ch.points || 0));
        
        // For markdown, we trust the marked library but still validate the input
        const safeDescription = ch.description ? 
            marked.parse(SecurityUtils.sanitizeInput(ch.description, UI_CONFIG.MAX_DESCRIPTION_LENGTH)) : 
            '';
        
        const solvedBadge = isSolved ? 
            '<span class="text-green-400 flex items-center gap-1"><svg class="w-4 h-4" fill="currentColor" viewBox="0 0 20 20"><path fill-rule="evenodd" d="M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z" clip-rule="evenodd"></path></svg>Solved</span>' : 
            '';

        return `
            <div class="bg-slate-800/50 border border-slate-700 rounded-lg p-8">
                <div class="flex items-start justify-between mb-6">
                    <div>
                        <h1 class="text-3xl font-bold text-white mb-2">${safeTitle}</h1>
                        <div class="flex items-center gap-4">
                            <span class="inline-block px-3 py-1 text-sm font-medium rounded-full ${this.getCategoryColor(ch.category)}">
                                ${safeCategory}
                            </span>
                            <span class="text-${this.getDifficultyColor(ch.difficulty)}-400 text-sm font-medium">${safeDifficulty}</span>
                            <span class="text-green-400 font-semibold">${safePoints} pts</span>
                            ${solvedBadge}
                        </div>
                    </div>
                </div>

                <div class="mb-6">
                    <h2 class="text-xl font-semibold text-white mb-3">Description</h2>
                    <div class="px-6 pb-2 text-white/75" style="text-wrap:balance">${safeDescription}</div>
                </div>

                ${this.generateHintsSection(ch)}
                ${this.generateSubmissionSection(ch, isSolved)}
            </div>
        `;
    },

    /**
     * Generate hints section HTML
     * @param {Object} ch - Challenge data
     * @returns {string} Hints section HTML
     */
    generateHintsSection(ch) {
        if (!ch.hints?.length) return '';
        
        const hintsHtml = ch.hints.map((h, index) => {
            const hintKey = `${ch.id}:${h.id}`;
            const isRevealed = AppState.revealedHints.has(hintKey);
            const isLocked = index > 0 && !AppState.revealedHints.has(`${ch.id}:${ch.hints[index - 1].id}`);
            
            if (isRevealed) {
                return `
                    <div class="my-2 p-4 border border-green-600 rounded-lg bg-green-900/20">
                        <div class="text-green-400 font-medium mb-2">Hint ${index + 1} (-${h.cost} pts)</div>
                        <div class="text-white/90">${SecurityUtils.escapeHtml(h.text || '')}</div>
                    </div>
                `;
            } else if (isLocked) {
                return `
                    <div class="my-2 p-4 border border-slate-700 rounded-lg opacity-30 cursor-not-allowed">
                        <em class="text-slate-400">Locked – reveal earlier hints first</em>
                    </div>
                `;
            } else {
                return `
                    <div class="my-2 p-4 border border-slate-600 rounded-lg bg-slate-800/30">
                        <div class="flex items-center justify-between">
                            <span class="text-slate-300">Hint ${index + 1}</span>
                            <button class="reveal-hint-btn bg-blue-600 hover:bg-blue-700 text-white px-3 py-1 rounded text-sm font-medium transition-colors"
                                    data-ch="${ch.id}" data-hint="${h.id}">
                                Reveal (-${h.cost} pts)
                            </button>
                        </div>
                    </div>
                `;
            }
        }).join('');
        
        return `
            <div class="mb-6">
                <h2 class="text-xl font-semibold text-white mb-3">Hints</h2>
                ${hintsHtml}
            </div>
        `;
    },

    /**
     * Generate submission section HTML
     * @param {Object} ch - Challenge data
     * @param {boolean} isSolved - Whether challenge is solved
     * @returns {string} Submission section HTML
     */
    generateSubmissionSection(ch, isSolved) {
        if (isSolved) {
            return `
                <div class="bg-green-900/20 border border-green-600 rounded-lg p-6">
                    <div class="flex items-center gap-3 text-green-400">
                        <svg class="w-6 h-6" fill="currentColor" viewBox="0 0 20 20">
                            <path fill-rule="evenodd" d="M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z" clip-rule="evenodd"></path>
                        </svg>
                        <span class="font-semibold text-lg">Challenge Solved!</span>
                    </div>
                </div>
            `;
        }

        return `
            <div class="bg-slate-700/30 rounded-lg p-6">
                <h2 class="text-xl font-semibold text-white mb-4">Submit Flag</h2>
                <form id="flag-form-${ch.id}" class="space-y-4">
                    <div>
                        <input type="text" id="flag-input-${ch.id}" 
                               class="w-full px-4 py-2 bg-slate-800 border border-slate-600 rounded-lg text-white placeholder-slate-400 focus:outline-none focus:border-blue-500"
                               placeholder="Enter FLAG text here" autocomplete="off" required>
                    </div>
                    <div class="flex items-center gap-4">
                        <button type="submit" 
                                class="bg-blue-600 hover:bg-blue-700 text-white px-6 py-2 rounded-lg font-medium transition-colors">
                            Submit
                        </button>
                        <div id="flag-result-${ch.id}"></div>
                    </div>
                </form>
            </div>
        `;
    },

    /**
     * Get category icon HTML
     * @param {string} category - Category name
     * @returns {string} Icon HTML
     */
    getCategoryIcon(category) {
        const icons = {
            'Web': '<svg class="w-4 h-4 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 12a9 9 0 01-9 9m9-9a9 9 0 00-9-9m9 9H3m9 9v-9m0-9v9m0 9c-5 0-9-4-9-9s4-9 9-9"></path></svg>',
            'Binary Exploitation': '<svg class="w-4 h-4 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 9l3 3-3 3m5 0h3M5 20h14a2 2 0 002-2V6a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z"></path></svg>',
            'Cryptography': '<svg class="w-4 h-4 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z"></path></svg>',
            'Forensics': '<svg class="w-4 h-4 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"></path></svg>',
            'Reverse Engineering': '<svg class="w-4 h-4 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"></path></svg>'
        };
        return icons[category] || '<svg class="w-4 h-4 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 21a4 4 0 01-4-4V5a2 2 0 012-2h4a2 2 0 012 2v12a4 4 0 01-4 4zm0 0h12a2 2 0 002-2v-4a2 2 0 00-2-2h-2.343M11 7.343l1.657-1.657a2 2 0 012.828 0l2.829 2.829a2 2 0 010 2.828l-8.486 8.485M7 17h.01"></path></svg>';
    },

    /**
     * Get category icon background color class
     * @param {string} category - Category name
     * @returns {string} CSS class
     */
    getCategoryIconBg(category) {
        const colors = {
            'Web': 'bg-blue-500',
            'Cryptography': 'bg-purple-500',
            'Binary Exploitation': 'bg-red-500',
            'Forensics': 'bg-yellow-500',
            'Reverse Engineering': 'bg-green-500'
        };
        return colors[category] || 'bg-gray-500';
    },

    /**
     * Get category color classes
     * @param {string} category - Category name
     * @returns {string} CSS classes
     */
    getCategoryColor(category) {
        const colors = {
            'Web': 'bg-blue-500/20 text-blue-400',
            'Cryptography': 'bg-purple-500/20 text-purple-400',
            'Binary Exploitation': 'bg-red-500/20 text-red-400',
            'Forensics': 'bg-yellow-500/20 text-yellow-400',
            'Reverse Engineering': 'bg-green-500/20 text-green-400'
        };
        return colors[category] || 'bg-gray-500/20 text-gray-400';
    },

    /**
     * Get difficulty color
     * @param {string} difficulty - Difficulty level
     * @returns {string} Color name
     */
    getDifficultyColor(difficulty) {
        const colors = {
            'Easy': 'green',
            'Medium': 'yellow',
            'Hard': 'red',
            'Expert': 'purple'
        };
        return colors[difficulty] || 'gray';
    }
};

/* =================================================================== */
/* SCOREBOARD MANAGEMENT                                               */
/* =================================================================== */

/**
 * Scoreboard and live scoring functionality
 */
const ScoreboardManager = {
    /**
     * Handle incoming score events from WebSocket
     * @param {Object} msg - Score event message
     * @param {boolean} defer - Whether to defer chart updates
     */
    handleScoreEvent(msg, defer = false) {
        const { ts: timestamp, displayname: name, points: delta, id: eventId } = msg;
        
        if (!timestamp || !name || delta == null || !eventId) return;
        if (AppState.seenEventIDs.has(eventId)) return;
        
        AppState.seenEventIDs.add(eventId);
        
        // Update timeline data
        this.updateTimeline(name, timestamp, delta, msg);
        
        // Update current scoreboard
        const currentScore = AppState.scoreboard.get(name) || 0;
        AppState.scoreboard.set(name, currentScore + delta);
        
        // Update user points in navbar if this is the current user
        if (name === AppState.currentUser) {
            const userPoints = AppState.scoreboard.get(AppState.currentUser) || 0;
            const pointsElement = document.getElementById('user-points');
            if (pointsElement) {
                pointsElement.textContent = `${userPoints} pts`;
            }
        }
        
        // Update UI unless deferred
        if (!defer) {
            this.flushChart();
        }
    },

    /**
     * Update timeline data for a user
     * @param {string} name - User display name
     * @param {number} timestamp - Event timestamp
     * @param {number} delta - Points delta
     * @param {Object} msg - Full message object
     */
    updateTimeline(name, timestamp, delta, msg) {
        if (!AppState.timelines.has(name)) {
            AppState.timelines.set(name, []);
        }
        
        const timeline = AppState.timelines.get(name);
        const lastScore = timeline.length > 0 ? timeline[timeline.length - 1].y : 0;
        
        const dataPoint = {
            x: new Date(timestamp), // timestamp is already in milliseconds
            y: lastScore + delta
        };
        
        // Add challenge name if available
        if (msg.challenge) {
            dataPoint.challenge = SecurityUtils.escapeHtml(msg.challenge);
        }
        
        timeline.push(dataPoint);
    },

    /**
     * Update user points in navbar from current scoreboard data
     */
    updateUserPoints() {
        if (!AppState.currentUser) return;
        
        // Find the current user's points in the scoreboard
        const userPoints = AppState.scoreboard.get(AppState.currentUser) || 0;
        const pointsElement = document.getElementById('user-points');
        if (pointsElement) {
            pointsElement.textContent = `${userPoints} pts`;
        }
    },

    /**
     * Flush chart updates to the UI
     */
    flushChart() {
        if (AppState.currentView === 'scoreboard') {
            this.refreshScoreboard();
            this.updateChart();
        }
    },

    /**
     * Refresh the scoreboard table
     */
    refreshScoreboard() {
        const tbody = document.getElementById('scoreboard-body');
        if (!tbody) return;
        
        tbody.innerHTML = '';
        
        const rows = [...AppState.scoreboard.entries()]
            .map(([name, score]) => ({ name, score, ts: this.getLastUpdateTs(name) }))
            .sort((a, b) => {
                if (b.score !== a.score) return b.score - a.score;
                return a.ts - b.ts;
            });

        // Show ALL players in the table (no slice)
        rows.forEach((row, idx) => {
            const tr = this.createScoreboardRow(row, idx + 1);
            tbody.appendChild(tr);
        });
    },

    /**
     * Create scoreboard table row
     * @param {Object} row - Row data
     * @param {number} rank - User rank
     * @returns {HTMLElement} Table row element
     */
    createScoreboardRow(row, rank) {
        const tr = document.createElement('tr');
        tr.className = 'border-b border-slate-700';
        
        const safeName = SecurityUtils.escapeHtml(row.name);
        const safeScore = SecurityUtils.escapeHtml(String(row.score));
        
        tr.innerHTML = `
            <td class="py-3 px-4 text-slate-300 font-medium">${rank}</td>
            <td class="py-3 px-4 text-white font-semibold">${safeName}</td>
            <td class="py-3 px-4 text-green-400 font-bold text-right">${safeScore}</td>
        `;
        
        return tr;
    },

    /**
     * Get last update timestamp for a user
     * @param {string} name - User display name
     * @returns {number} Last update timestamp
     */
    getLastUpdateTs(name) {
        const timeline = AppState.timelines.get(name);
        return timeline && timeline.length > 0 ? 
            timeline[timeline.length - 1].x.getTime() : 
            0;
    },

    /**
     * Update the score chart
     */
    updateChart() {
        if (!AppState.scoreChart) {
            this.initChart();
        }
        
        const datasets = this.createChartDatasets();
        AppState.scoreChart.data.datasets = datasets;
        AppState.scoreChart.update('none');
    },

    /**
     * Create chart datasets from timeline data
     * @returns {Array} Chart.js datasets
     */
    createChartDatasets() {
        const topUsers = [...AppState.scoreboard.entries()]
            .sort((a, b) => b[1] - a[1])
            .slice(0, SCOREBOARD_CONFIG.TOP_N)
            .map(([name]) => name);

        return topUsers.map((name, idx) => {
            const timeline = AppState.timelines.get(name) || [];
            return {
                label: SecurityUtils.escapeHtml(name),
                data: timeline,
                borderColor: SCOREBOARD_CONFIG.CHART_COLORS[idx % SCOREBOARD_CONFIG.CHART_COLORS.length],
                backgroundColor: SCOREBOARD_CONFIG.CHART_COLORS[idx % SCOREBOARD_CONFIG.CHART_COLORS.length] + '20',
                tension: 0.2,
                pointRadius: 3,
                pointHoverRadius: 5
            };
        });
    },

    /**
     * Initialize the score chart
     */
    initChart() {
        const ctx = document.getElementById('score-chart');
        if (!ctx) return;
        
        AppState.scoreChart = new Chart(ctx.getContext('2d'), {
            type: 'line',
            data: { datasets: [] },
            options: {
                animation: true,
                responsive: true,
                maintainAspectRatio: false,
                plugins: { 
                    legend: { labels: { color: '#e2e8f0' } },
                    tooltip: {
                        callbacks: {
                            label(ctx) {
                                return `${ctx.dataset.label}: ${ctx.parsed.y} pts`;
                            },
                            afterLabel(ctx) {
                                const title = ctx.raw.challenge;
                                return title ? `🏁 ${title}` : '';
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
                        grid: { color: 'rgba(148,163,184,0.1)' }
                    },
                    y: {
                        beginAtZero: true,
                        ticks: { color: '#cbd5e1' },
                        grid: { color: 'rgba(148,163,184,0.1)' }
                    }
                }
            }
        });
    }
};

/* =================================================================== */
/* EVENT LISTENERS & INITIALIZATION                                    */
/* =================================================================== */

/**
 * Set up event listeners and initialize the application
 */
function initializeApplication() {
    // Initialize the scoreboard chart
    ScoreboardManager.initChart();
    
    // Authentication event listeners
    document.getElementById('login-form')?.addEventListener('submit', AuthManager.handleLogin.bind(AuthManager));
    document.getElementById('logout-btn')?.addEventListener('click', AuthManager.handleLogout.bind(AuthManager));
    
    // Navigation event listeners
    document.getElementById('challenges-btn')?.addEventListener('click', () => {
        ViewManager.showView('challenges', true);
    });
    document.getElementById('scoreboard-btn')?.addEventListener('click', () => {
        ViewManager.showView('scoreboard', true);
    });
    document.getElementById('back-btn')?.addEventListener('click', () => {
        ViewManager.showView('challenges', true);
    });
    
    // Browser navigation handling
    window.addEventListener('popstate', (e) => {
        if (e.state?.view) {
            if (e.state.view === 'challenge') {
                ViewManager.showChallenge(e.state.challengeId);
            } else {
                ViewManager.showView(e.state.view);
            }
        }
    });
    
    // Initial route handling
    handleInitialRoute();
}

/**
 * Handle initial route from URL hash
 */
function handleInitialRoute() {
    const hash = location.hash.slice(1);
    
    if (hash === 'scoreboard') {
        ViewManager.showView('scoreboard');
    } else if (hash.startsWith('challenge-')) {
        const id = Number(hash.split('-')[1]);
        if (id) ViewManager.showChallenge(id);
    }
    // Default to challenges view
}

/* =================================================================== */
/* APPLICATION STARTUP                                                 */
/* =================================================================== */

// Initialize the application when DOM is ready
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initializeApplication);
} else {
    initializeApplication();
}

// Export for debugging (development only)
if (typeof window !== 'undefined') {
    window.CTFG = {
        AppState,
        AuthManager,
        WebSocketManager,
        ViewManager,
        ChallengeManager,
        ChallengeRenderer,
        ScoreboardManager,
        SecurityUtils,
        UIUtils
    };
}