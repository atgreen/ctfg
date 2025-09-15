#!/usr/bin/env node

const fs = require('fs');
const WebSocket = require('ws');

// Global metrics collection
const METRICS = {
    startTime: Date.now(),
    endTime: null,
    players: [],
    totals: {
        loginAttempts: 0,
        loginSuccess: 0,
        wsConnections: 0,
        wsSuccessful: 0,
        flagSubmissions: 0,
        flagsCorrect: 0,
        challengesLoaded: 0,
        staticFilesLoaded: 0,
        errors: []
    },
    timeline: [], // Time-series data
    responseTimes: {
        staticFiles: [],
        login: [],
        challenges: [],
        submit: [],
        websocket: []
    },
    staticFileBreakdown: {
        'index.html': [],
        'CSS': [],
        'banner image': [],
        'app.js': []
    }
};

// Parse command line arguments
const args = process.argv.slice(2);
if (args.length < 3) {
    console.error('Usage: node player-emulator.js <server_url> <challenges.json> <credentials.csv> [num_players] [jitter_seconds]');
    console.error('Example: node player-emulator.js http://localhost:9090 challenges.json credentials.csv 5 10');
    console.error('  - jitter_seconds: spread player start times over N seconds (default: 10, use 0 for thundering herd)');
    process.exit(1);
}

const SERVER_URL = args[0].replace(/\/$/, ''); // Remove trailing slash
const CHALLENGES_FILE = args[1];
const CREDENTIALS_FILE = args[2];
const NUM_PLAYERS = parseInt(args[3]) || 1;
const JITTER_SECONDS = parseFloat(args[4]) || 10;

// Load challenges - handle non-standard JSON with actual newlines in strings
let challengesRaw = fs.readFileSync(CHALLENGES_FILE, 'utf8');

// Fix non-standard JSON by escaping newlines within string values
// This regex finds strings and escapes newlines within them
challengesRaw = challengesRaw.replace(/"([^"\\]*(\\.[^"\\]*)*)"/g, (match) => {
    // Replace actual newlines with \n escape sequence within the matched string
    return match.replace(/\n/g, '\\n').replace(/\r/g, '\\r');
});

const challenges = JSON.parse(challengesRaw);
console.log(`Loaded ${challenges.length} challenges`);

// Load credentials
const credentialsRaw = fs.readFileSync(CREDENTIALS_FILE, 'utf8');
const credentials = credentialsRaw.split('\n')
    .filter(line => line.trim())  // Filter empty lines
    .filter(line => !line.startsWith('username'))  // Skip header row
    .map(line => {
        const [username, password] = line.split(',').map(s => s.trim());
        return { username, password };
    })
    .filter(cred => cred.username && cred.password && cred.username !== 'player1'); // Skip player1 and invalid entries
console.log(`Loaded ${credentials.length} credentials (excluding player1)`);

if (NUM_PLAYERS > credentials.length) {
    console.error(`Error: Requested ${NUM_PLAYERS} players but only ${credentials.length} credentials available`);
    process.exit(1);
}

// Helper function to make HTTP requests with timing and timeout
async function httpRequest(url, options = {}) {
    const http = url.startsWith('https') ? require('https') : require('http');
    const startTime = Date.now();
    const timeout = options.timeout || 30000; // 30 second default timeout

    return new Promise((resolve, reject) => {
        const urlObj = new URL(url);
        const reqOptions = {
            hostname: urlObj.hostname,
            port: urlObj.port || (urlObj.protocol === 'https:' ? 443 : 80),
            path: urlObj.pathname + urlObj.search,
            method: options.method || 'GET',
            headers: options.headers || {},
            timeout: timeout,
            ...options
        };

        const req = http.request(reqOptions, (res) => {
            let data = '';
            res.setTimeout(timeout);
            res.on('data', chunk => data += chunk);
            res.on('end', () => {
                const responseTime = Date.now() - startTime;
                resolve({
                    status: res.statusCode,
                    headers: res.headers,
                    data: data,
                    cookies: res.headers['set-cookie'],
                    responseTime: responseTime,
                    timestamp: startTime
                });
            });
            res.on('timeout', () => {
                req.destroy();
                const responseTime = Date.now() - startTime;
                reject(new Error(`HTTP response timeout after ${timeout}ms`));
            });
        });

        req.setTimeout(timeout, () => {
            req.destroy();
            const responseTime = Date.now() - startTime;
            reject(new Error(`HTTP request timeout after ${timeout}ms`));
        });

        req.on('error', (err) => {
            const responseTime = Date.now() - startTime;
            err.responseTime = responseTime;
            err.timestamp = startTime;
            reject(err);
        });

        if (options.body) {
            req.write(options.body);
        }
        req.end();
    });
}

// Metrics recording functions
function recordMetric(category, value, playerId = null) {
    const timestamp = Date.now();
    METRICS.timeline.push({
        timestamp,
        category,
        value,
        playerId
    });
}

function recordError(error, context, playerId = null) {
    METRICS.totals.errors.push({
        timestamp: Date.now(),
        error: error.message || error,
        context,
        playerId
    });
}

// Sleep helper
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));

// Random delay between actions (minimal for worst-case stress testing)
const randomDelay = () => sleep(Math.floor(Math.random() * 100) + 50);

class Player {
    constructor(id, username, password) {
        this.id = id;
        this.username = username;
        this.password = password;
        this.displayName = `Player_${username}_${Date.now() % 1000}`;
        this.sessionCookie = null;
        this.ws = null;
        this.wsConnected = false;
        this.wsConnectTime = null;
        this.solvedChallenges = new Set();
        this.availableChallenges = [];

        // Player-specific metrics
        this.metrics = {
            startTime: Date.now(),
            loginTime: null,
            wsConnectTime: null,
            firstChallengeTime: null,
            challengesSolved: 0,
            flagsSubmitted: 0,
            errors: []
        };

        // Add to global metrics
        METRICS.players.push(this);
    }

    log(message) {
        console.log(`[Player ${this.id}/${this.username}] ${message}`);
    }

    async fetchStaticFiles() {
        try {
            const files = [
                { url: `${SERVER_URL}/`, name: 'index.html' },
                { url: `${SERVER_URL}/css/ctfg.css`, name: 'CSS' },
                { url: `${SERVER_URL}/images/banner.png`, name: 'banner image' },
                { url: `${SERVER_URL}/js/app.js`, name: 'app.js' }
            ];

            for (const file of files) {
                const response = await httpRequest(file.url);
                METRICS.responseTimes.staticFiles.push(response.responseTime);
                METRICS.staticFileBreakdown[file.name].push(response.responseTime);
                METRICS.totals.staticFilesLoaded++;
                recordMetric('static_file_loaded', response.responseTime, this.id);
                this.log(`Fetched ${file.name} (${response.responseTime}ms)`);
            }
        } catch (err) {
            this.log(`Error fetching static files: ${err.message}`);
            recordError(err, 'static_files', this.id);
            this.metrics.errors.push({timestamp: Date.now(), error: err.message, context: 'static_files'});
        }
    }

    async login() {
        try {
            this.log(`Attempting login with username: ${this.username}, password: ${this.password.substring(0,3)}...`);
            METRICS.totals.loginAttempts++;

            const response = await httpRequest(`${SERVER_URL}/api/login`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    username: this.username,
                    password: this.password
                })
            });

            METRICS.responseTimes.login.push(response.responseTime);
            recordMetric('login_attempt', response.responseTime, this.id);

            if (response.status === 200) {
                this.sessionCookie = response.cookies ? response.cookies[0] : null;
                const data = JSON.parse(response.data);
                this.log(`Logged in successfully (${response.responseTime}ms). Needs name: ${data.needs_name}`);

                this.metrics.loginTime = Date.now();
                METRICS.totals.loginSuccess++;
                recordMetric('login_success', 1, this.id);

                // Extract WebSocket URL
                const wsUrl = data.websocket_url || `ws://localhost:12345`;
                this.wsUrl = wsUrl;
                this.log(`WebSocket URL from server: ${wsUrl}`);

                return true;
            } else {
                this.log(`Login failed with status ${response.status} (${response.responseTime}ms): ${response.data}`);
                recordError(`Login failed: ${response.status}`, 'login', this.id);
                this.metrics.errors.push({timestamp: Date.now(), error: `Login failed: ${response.status}`, context: 'login'});
                return false;
            }
        } catch (err) {
            this.log(`Login error: ${err.message}`);
            recordError(err, 'login', this.id);
            this.metrics.errors.push({timestamp: Date.now(), error: err.message, context: 'login'});
            return false;
        }
    }

    async setDisplayName() {
        try {
            const response = await httpRequest(`${SERVER_URL}/api/set-name`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                    'Cookie': this.sessionCookie
                },
                body: JSON.stringify({
                    name: this.displayName
                })
            });

            if (response.status === 200) {
                this.log(`Display name set to: ${this.displayName}`);
                return true;
            } else if (response.status === 409) {
                // Name taken, try another
                this.displayName = `Player_${this.username}_${Date.now()}`;
                this.log(`Name taken, retrying with: ${this.displayName}`);
                return await this.setDisplayName();
            } else {
                this.log(`Set name failed with status ${response.status}`);
                return false;
            }
        } catch (err) {
            this.log(`Set name error: ${err.message}`);
            return false;
        }
    }

    connectWebSocket() {
        return new Promise((resolve, reject) => {
            // Check if wsUrl already ends with /scorestream
            const wsUrl = this.wsUrl.endsWith('/scorestream')
                ? this.wsUrl
                : `${this.wsUrl}/scorestream`;
            this.log(`Connecting to WebSocket: ${wsUrl}`);

            METRICS.totals.wsConnections++;
            const wsConnectStart = Date.now();

            // Set a timeout for WebSocket connection
            const wsTimeout = setTimeout(() => {
                if (!this.wsConnected) {
                    this.log('WebSocket connection timeout');
                    recordError('WebSocket connection timeout', 'websocket', this.id);
                    if (this.ws) {
                        this.ws.close();
                    }
                    reject(new Error('WebSocket connection timeout'));
                }
            }, 15000); // 15 second timeout

            this.ws = new WebSocket(wsUrl);

            this.ws.on('open', () => {
                clearTimeout(wsTimeout); // Clear timeout on successful connection
                const connectTime = Date.now() - wsConnectStart;
                this.log(`WebSocket connected (${connectTime}ms)`);
                this.wsConnected = true;
                this.metrics.wsConnectTime = Date.now();

                METRICS.totals.wsSuccessful++;
                METRICS.responseTimes.websocket.push(connectTime);
                recordMetric('websocket_connected', connectTime, this.id);

                // Send periodic pings to keep connection alive
                this.pingInterval = setInterval(() => {
                    if (this.wsConnected && this.ws.readyState === WebSocket.OPEN) {
                        this.ws.send('ping');
                    }
                }, 30000);

                resolve();
            });

            this.ws.on('message', (data) => {
                try {
                    const message = data.toString();

                    // Try to parse as JSON
                    let events;
                    try {
                        events = JSON.parse(message);
                    } catch (e) {
                        // Not JSON, could be a ping response or other message
                        if (message !== 'pong' && message.length > 0) {
                            this.log(`Non-JSON WebSocket message: ${message.substring(0, 100)}`);
                        }
                        return;
                    }

                    // Validate structure
                    if (!Array.isArray(events)) {
                        this.log(`WebSocket message not an array: ${JSON.stringify(events).substring(0, 200)}`);
                        return;
                    }

                    // Process each event
                    for (const event of events) {
                        // Validate event structure
                        if (!event || typeof event !== 'object') {
                            this.log(`Invalid event object: ${JSON.stringify(event)}`);
                            continue;
                        }

                        // Log all event types for debugging
                        if (event.type) {
                            if (event.type === 'score') {
                                // Validate score event fields
                                if (!event.displayname || !event.points || !event.challenge) {
                                    this.log(`Incomplete score event: ${JSON.stringify(event)}`);
                                    continue;
                                }

                                if (event.displayname === this.displayName) {
                                    this.log(`✓ My score: +${event.points} for "${event.challenge}" (id: ${event.id}, ts: ${event.ts})`);
                                } else {
                                    // Log other players' scores at debug level
                                    if (Math.random() < 0.1) { // Sample 10% to reduce noise
                                        this.log(`Other player: ${event.displayname} scored ${event.points}`);
                                    }
                                }
                            } else if (event.type === 'hint') {
                                if (event.displayname === this.displayName) {
                                    this.log(`Hint purchased: -${Math.abs(event.points)} points`);
                                }
                            } else {
                                this.log(`Unknown event type: ${event.type}`);
                            }
                        } else {
                            this.log(`Event missing type field: ${JSON.stringify(event).substring(0, 200)}`);
                        }
                    }
                } catch (err) {
                    this.log(`Error processing WebSocket message: ${err.message}`);
                }
            });

            this.ws.on('close', () => {
                this.log('WebSocket disconnected');
                this.wsConnected = false;
                if (this.pingInterval) {
                    clearInterval(this.pingInterval);
                }
            });

            this.ws.on('error', (err) => {
                clearTimeout(wsTimeout);
                this.log(`WebSocket error: ${err.message}`);
                this.wsConnected = false;
                recordError(err, 'websocket', this.id);
                this.metrics.errors.push({timestamp: Date.now(), error: err.message, context: 'websocket'});
                reject(err);
            });
        });
    }

    async getChallenges() {
        try {
            const response = await httpRequest(`${SERVER_URL}/api/challenges`, {
                headers: {
                    'Cookie': this.sessionCookie
                }
            });

            if (response.status === 200) {
                const data = JSON.parse(response.data);
                this.availableChallenges = data;
                this.log(`Retrieved ${data.length} available challenges`);
                return data;
            } else {
                this.log(`Get challenges failed with status ${response.status}`);
                return [];
            }
        } catch (err) {
            this.log(`Get challenges error: ${err.message}`);
            return [];
        }
    }

    async submitFlag(challengeId, flag) {
        try {
            METRICS.totals.flagSubmissions++;
            this.metrics.flagsSubmitted++;

            const response = await httpRequest(`${SERVER_URL}/api/submit`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                    'Cookie': this.sessionCookie
                },
                body: JSON.stringify({
                    id: challengeId,
                    flag: flag
                })
            });

            METRICS.responseTimes.submit.push(response.responseTime);
            recordMetric('submit_flag', response.responseTime, this.id);

            const data = JSON.parse(response.data);
            if (response.status === 200) {
                if (data.result === 'correct') {
                    this.log(`✓ Challenge ${challengeId} solved! (+${data.points} points, total: ${data.total}) [${response.responseTime}ms]`);
                    this.solvedChallenges.add(challengeId);
                    this.metrics.challengesSolved++;
                    METRICS.totals.flagsCorrect++;

                    if (this.metrics.firstChallengeTime === null) {
                        this.metrics.firstChallengeTime = Date.now();
                    }

                    recordMetric('flag_correct', 1, this.id);

                    // Emulate browser: reload challenges after successful submission
                    await this.getChallenges();

                    return true;
                } else if (data.result === 'already_solved') {
                    this.log(`Challenge ${challengeId} already solved [${response.responseTime}ms]`);
                    this.solvedChallenges.add(challengeId);
                    return true;
                } else {
                    this.log(`✗ Challenge ${challengeId} incorrect flag [${response.responseTime}ms]`);
                    recordMetric('flag_incorrect', 1, this.id);
                    return false;
                }
            } else {
                this.log(`Submit failed with status ${response.status} [${response.responseTime}ms]: ${response.data}`);
                recordError(`Submit failed: ${response.status}`, 'submit', this.id);
                return false;
            }
        } catch (err) {
            this.log(`Submit error: ${err.message}`);
            recordError(err, 'submit', this.id);
            this.metrics.errors.push({timestamp: Date.now(), error: err.message, context: 'submit'});
            return false;
        }
    }

    async solveChallenges() {
        let solvedCount = 0;
        let maxIterations = 50; // Prevent infinite loops
        let consecutiveFailures = 0;
        const startTime = Date.now();
        const maxDuration = 120000; // 2 minute timeout for challenge solving

        // Get initial challenges
        let availableChallenges = await this.getChallenges();

        while (maxIterations-- > 0) {
            // Check timeout
            if (Date.now() - startTime > maxDuration) {
                this.log(`Challenge solving timeout after ${maxDuration/1000}s`);
                break;
            }

            // Find unsolved challenges from our cached list
            const unsolved = availableChallenges.filter(c => !c.solved);

            if (unsolved.length === 0) {
                this.log('All challenges solved!');
                break;
            }

            // Submit multiple challenges concurrently (up to 3 at a time)
            const batch = unsolved.slice(0, 3);
            const submissions = batch.map(async (challenge) => {
                const originalChallenge = challenges.find(c => c.id === challenge.id);
                if (!originalChallenge || !originalChallenge.testflag) {
                    this.log(`No test flag found for challenge ${challenge.id}`);
                    return false;
                }

                await randomDelay(); // Small random delay per submission
                return await this.submitFlag(challenge.id, originalChallenge.testflag);
            });

            try {
                const results = await Promise.race([
                    Promise.all(submissions),
                    new Promise((_, reject) => setTimeout(() => reject(new Error('Batch timeout')), 30000))
                ]);

                const successCount = results.filter(r => r).length;
                solvedCount += successCount;

                if (successCount === 0) {
                    consecutiveFailures++;
                    if (consecutiveFailures > 5) {
                        this.log('Too many consecutive failures, waiting a bit...');
                        await sleep(2000);
                        consecutiveFailures = 0;
                        // Refresh challenges after failures
                        availableChallenges = await this.getChallenges();
                    }
                } else {
                    consecutiveFailures = 0;
                    // Update our local cache with the refreshed list from submitFlag
                    availableChallenges = this.availableChallenges;
                }
            } catch (err) {
                this.log(`Batch submission error: ${err.message}`);
                consecutiveFailures++;
            }

            // Minimal delay between batches (worst case stress testing)
            await sleep(100);
        }

        this.log(`Completed solving challenges. Total solved: ${solvedCount}`);
    }

    async run() {
        // Wrap entire player run in a timeout
        const playerTimeout = 300000; // 5 minute total timeout per player

        try {
            await Promise.race([
                this._runInternal(),
                new Promise((_, reject) =>
                    setTimeout(() => reject(new Error(`Player ${this.id} timeout after ${playerTimeout/1000}s`)), playerTimeout)
                )
            ]);
        } catch (err) {
            this.log(`Player run failed: ${err.message}`);
            recordError(err, 'player_run', this.id);
            this.metrics.errors.push({timestamp: Date.now(), error: err.message, context: 'player_run'});
        } finally {
            // Always cleanup WebSocket and timers
            await this.cleanup();
            this.log('Player simulation complete');
        }
    }

    async _runInternal() {
        this.log('Starting player simulation...');

        // First fetch static files (like a browser loading the page)
        await this.fetchStaticFiles();

        // Minimal delay to simulate page rendering (worst case)
        await sleep(100);

        // Then login (user would see the page first, then login)
        const loginResult = await this.login();
        if (!loginResult) {
            this.log('Failed to login, aborting');
            return;
        }

        // Minimal delay after login (worst case)
        await sleep(50);

        // Set display name and connect WebSocket concurrently
        // (These happen together once logged in)
        try {
            await Promise.all([
                this.setDisplayName(),
                this.connectWebSocket().catch(err => {
                    this.log(`WebSocket connection failed: ${err.message}`);
                    // Continue without WebSocket
                })
            ]);
        } catch (err) {
            this.log(`Setup error: ${err.message}, continuing anyway`);
        }

        // Minimal delay before starting to solve (worst case)
        await sleep(200);

        // Start solving challenges
        await this.solveChallenges();

        // Keep WebSocket alive for a bit
        this.log('Keeping connection alive...');
        await sleep(10000);
    }

    async cleanup() {
        this.log('Cleaning up resources...');

        // Clear ping interval first
        if (this.pingInterval) {
            clearInterval(this.pingInterval);
            this.pingInterval = null;
        }

        // Close WebSocket connection properly
        if (this.ws) {
            try {
                // Mark as disconnected to prevent further operations
                this.wsConnected = false;

                // Only close if not already closed/closing
                if (this.ws.readyState === WebSocket.OPEN || this.ws.readyState === WebSocket.CONNECTING) {
                    this.ws.close();

                    // Wait a bit for the close to complete
                    await new Promise(resolve => {
                        const closeTimeout = setTimeout(resolve, 1000); // 1 second max wait

                        const handleClose = () => {
                            clearTimeout(closeTimeout);
                            this.ws.removeAllListeners();
                            resolve();
                        };

                        if (this.ws.readyState === WebSocket.CLOSED) {
                            handleClose();
                        } else {
                            this.ws.once('close', handleClose);
                        }
                    });
                }

                // Remove all listeners to prevent memory leaks
                this.ws.removeAllListeners();

            } catch (err) {
                this.log(`WebSocket cleanup error: ${err.message}`);
            } finally {
                this.ws = null;
            }
        }

        this.log('Cleanup complete');
    }

    cleanupSync() {
        this.log('Cleaning up resources (sync)...');

        // Clear ping interval first
        if (this.pingInterval) {
            clearInterval(this.pingInterval);
            this.pingInterval = null;
        }

        // Close WebSocket connection synchronously
        if (this.ws) {
            try {
                // Mark as disconnected to prevent further operations
                this.wsConnected = false;

                // Only close if not already closed/closing
                if (this.ws.readyState === WebSocket.OPEN || this.ws.readyState === WebSocket.CONNECTING) {
                    this.ws.close();
                }

                // Remove all listeners to prevent memory leaks
                this.ws.removeAllListeners();
                this.ws = null;

            } catch (err) {
                this.log(`WebSocket cleanup error: ${err.message}`);
            }
        }

        this.log('Sync cleanup complete');
    }
}

// Global variables for cleanup
let progressInterval = null;
let isShuttingDown = false;
let globalPlayers = []; // Track players for cleanup during shutdown

// Clean metrics object of circular references for JSON serialization
function cleanMetricsForSerialization(metrics) {
    return JSON.parse(JSON.stringify(metrics, (key, value) => {
        // Filter out functions, circular references, and Node.js internal objects
        if (typeof value === 'function' ||
            value instanceof RegExp ||
            (value && typeof value === 'object' && value.constructor &&
             ['Timeout', 'TimersList', 'Socket', 'WebSocket'].includes(value.constructor.name))) {
            return undefined;
        }
        return value;
    }));
}

// Graceful shutdown handler
function gracefulShutdown(signal) {
    if (isShuttingDown) {
        console.log('\nForcing immediate exit...');
        process.exit(1);
    }

    isShuttingDown = true;
    console.log(`\n🛑 Received ${signal}, generating performance report...`);

    // Stop progress monitor
    if (progressInterval) {
        clearInterval(progressInterval);
    }

    // Cleanup all active players (sync version for signal handlers)
    console.log(`🧹 Cleaning up ${globalPlayers.length} player connections...`);
    for (const player of globalPlayers) {
        try {
            player.cleanupSync();
        } catch (err) {
            console.log(`Error cleaning up player ${player.id}: ${err.message}`);
        }
    }

    // Finalize metrics and generate report
    METRICS.endTime = Date.now();

    try {
        // Clean and save metrics to file
        const cleanedMetrics = cleanMetricsForSerialization(METRICS);
        const metricsFile = `metrics-interrupted-${Date.now()}.json`;
        fs.writeFileSync(metricsFile, JSON.stringify(cleanedMetrics, null, 2));
        console.log(`📊 Metrics saved to: ${metricsFile}`);

        // Generate performance report
        const { generatePerformanceReport } = require('./generate-performance-report.js');
        const reportFile = `performance-report-interrupted-${Date.now()}.html`;
        generatePerformanceReport(cleanedMetrics, reportFile);

        // Print quick summary
        const duration = Math.round((METRICS.endTime - METRICS.startTime) / 1000);
        const successRate = METRICS.totals.loginAttempts > 0 ?
            (METRICS.totals.loginSuccess / METRICS.totals.loginAttempts * 100).toFixed(1) : '0.0';

        console.log(`\n📋 INTERRUPTED TEST SUMMARY:`);
        console.log(`├─ Duration: ${duration}s (interrupted)`);
        console.log(`├─ Login Success: ${METRICS.totals.loginSuccess}/${METRICS.totals.loginAttempts} (${successRate}%)`);
        console.log(`├─ Flags Correct: ${METRICS.totals.flagsCorrect}`);
        console.log(`├─ Errors: ${METRICS.totals.errors.length}`);
        console.log(`└─ Report: ${reportFile}`);

    } catch (err) {
        console.error(`Error generating interrupted report: ${err.message}`);
    }

    console.log('\n👋 Graceful shutdown complete');
    process.exit(0);
}

// Register signal handlers
process.on('SIGINT', () => gracefulShutdown('SIGINT'));   // Ctrl-C
process.on('SIGTERM', () => gracefulShutdown('SIGTERM')); // kill command
process.on('SIGHUP', () => gracefulShutdown('SIGHUP'));   // terminal close

async function main() {
    console.log(`Starting ${NUM_PLAYERS} player emulation(s) against ${SERVER_URL}`);
    console.log(`Jitter: ${JITTER_SECONDS}s (${JITTER_SECONDS === 0 ? 'thundering herd' : 'staggered start'})`);
    console.log('Press Ctrl-C to interrupt and generate partial report');
    console.log('----------------------------------------');

    const players = [];

    // Create player instances
    for (let i = 0; i < NUM_PLAYERS; i++) {
        const cred = credentials[i];
        const player = new Player(i + 1, cred.username, cred.password);
        players.push(player);
        globalPlayers.push(player); // Track for graceful shutdown
    }

    // Start real-time progress monitor
    progressInterval = setInterval(() => {
        if (isShuttingDown) return; // Skip if shutting down

        const elapsed = Math.round((Date.now() - METRICS.startTime) / 1000);
        const loginRate = METRICS.totals.loginAttempts > 0 ?
            (METRICS.totals.loginSuccess / METRICS.totals.loginAttempts * 100).toFixed(1) : '0.0';
        const wsRate = METRICS.totals.wsConnections > 0 ?
            (METRICS.totals.wsSuccessful / METRICS.totals.wsConnections * 100).toFixed(1) : '0.0';

        process.stdout.write(`\r⏱️  ${elapsed}s | 👥 ${METRICS.totals.loginSuccess}/${NUM_PLAYERS} logged in (${loginRate}%) | 🔌 ${METRICS.totals.wsSuccessful} WS (${wsRate}%) | 🚩 ${METRICS.totals.flagsCorrect} flags | ❌ ${METRICS.totals.errors.length} errors`);
    }, 1000);

    // Run players with configurable start jitter
    console.log(`\n🎲 Using ${JITTER_SECONDS}s jitter window (0 = thundering herd, >0 = staggered start)`);

    const promises = players.map(async (player, index) => {
        if (JITTER_SECONDS > 0) {
            // Add random jitter to simulate realistic user behavior
            const jitterMs = Math.random() * JITTER_SECONDS * 1000;
            await sleep(jitterMs);
            console.log(`[Player ${player.id}] Starting after ${(jitterMs/1000).toFixed(1)}s jitter`);
        } else {
            // No jitter - true thundering herd for stress testing
            console.log(`[Player ${player.id}] Starting immediately (thundering herd mode)`);
        }
        return player.run();
    });

    // Wait for all players to complete with global timeout
    const globalTimeout = 600000; // 10 minute global timeout
    try {
        await Promise.race([
            Promise.all(promises),
            new Promise((_, reject) =>
                setTimeout(() => reject(new Error(`Global test timeout after ${globalTimeout/1000}s`)), globalTimeout)
            )
        ]);
    } catch (err) {
        console.log(`\n⚠️  Test timeout: ${err.message}`);
        console.log('Forcing completion of remaining players...');

        // Give any remaining players a few more seconds
        await Promise.race([
            Promise.allSettled(promises),
            new Promise(resolve => setTimeout(resolve, 5000))
        ]);
    }

    // Stop progress monitor
    clearInterval(progressInterval);
    console.log('\n----------------------------------------');
    console.log('All player simulations complete');

    // Finalize metrics
    METRICS.endTime = Date.now();

    // Cleanup all players at normal completion
    console.log(`\n🧹 Cleaning up ${globalPlayers.length} player connections...`);
    for (const player of globalPlayers) {
        try {
            await player.cleanup();
        } catch (err) {
            console.log(`Error cleaning up player ${player.id}: ${err.message}`);
        }
    }

    // Clean and save metrics to file
    const cleanedMetrics = cleanMetricsForSerialization(METRICS);
    const metricsFile = `metrics-${Date.now()}.json`;
    fs.writeFileSync(metricsFile, JSON.stringify(cleanedMetrics, null, 2));
    console.log(`📊 Metrics saved to: ${metricsFile}`);

    // Generate performance report
    const { generatePerformanceReport } = require('./generate-performance-report.js');
    const reportFile = `performance-report-${Date.now()}.html`;
    generatePerformanceReport(cleanedMetrics, reportFile);

    // Print summary
    const duration = Math.round((METRICS.endTime - METRICS.startTime) / 1000);
    const successRate = (METRICS.totals.loginSuccess / METRICS.totals.loginAttempts * 100).toFixed(1);

    console.log('\n🎯 PERFORMANCE SUMMARY:');
    console.log(`├─ Duration: ${duration}s`);
    console.log(`├─ Players: ${NUM_PLAYERS}`);
    console.log(`├─ Login Success: ${METRICS.totals.loginSuccess}/${METRICS.totals.loginAttempts} (${successRate}%)`);
    console.log(`├─ WebSocket Success: ${METRICS.totals.wsSuccessful}/${METRICS.totals.wsConnections} (${(METRICS.totals.wsSuccessful/METRICS.totals.wsConnections*100).toFixed(1)}%)`);
    console.log(`├─ Flags Submitted: ${METRICS.totals.flagSubmissions}`);
    console.log(`├─ Flags Correct: ${METRICS.totals.flagsCorrect} (${(METRICS.totals.flagsCorrect/METRICS.totals.flagSubmissions*100).toFixed(1)}%)`);
    console.log(`├─ Throughput: ${(METRICS.totals.flagsCorrect/duration).toFixed(2)} flags/second`);
    console.log(`├─ Errors: ${METRICS.totals.errors.length}`);
    console.log(`└─ Report: ${reportFile}\n`);

    // Print top errors if any
    if (METRICS.totals.errors.length > 0) {
        console.log('⚠️  TOP ERRORS:');
        const errorCounts = {};
        METRICS.totals.errors.forEach(err => {
            const key = `${err.context}: ${err.error}`;
            errorCounts[key] = (errorCounts[key] || 0) + 1;
        });

        Object.entries(errorCounts)
            .sort(([,a], [,b]) => b - a)
            .slice(0, 5)
            .forEach(([error, count]) => {
                console.log(`   ${count}x ${error}`);
            });
    }
}

// Run the main function
main().catch(err => {
    console.error('Fatal error:', err);
    process.exit(1);
});