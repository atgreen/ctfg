#!/usr/bin/env node

const fs = require('fs');
const WebSocket = require('ws');

// Parse command line arguments
const args = process.argv.slice(2);
if (args.length < 3) {
    console.error('Usage: node player-emulator.js <server_url> <challenges.json> <credentials.csv> [num_players]');
    console.error('Example: node player-emulator.js http://localhost:9090 challenges.json credentials.csv 5');
    process.exit(1);
}

const SERVER_URL = args[0].replace(/\/$/, ''); // Remove trailing slash
const CHALLENGES_FILE = args[1];
const CREDENTIALS_FILE = args[2];
const NUM_PLAYERS = parseInt(args[3]) || 1;

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

// Helper function to make HTTP requests
async function httpRequest(url, options = {}) {
    const http = url.startsWith('https') ? require('https') : require('http');

    return new Promise((resolve, reject) => {
        const urlObj = new URL(url);
        const reqOptions = {
            hostname: urlObj.hostname,
            port: urlObj.port || (urlObj.protocol === 'https:' ? 443 : 80),
            path: urlObj.pathname + urlObj.search,
            method: options.method || 'GET',
            headers: options.headers || {},
            ...options
        };

        const req = http.request(reqOptions, (res) => {
            let data = '';
            res.on('data', chunk => data += chunk);
            res.on('end', () => {
                resolve({
                    status: res.statusCode,
                    headers: res.headers,
                    data: data,
                    cookies: res.headers['set-cookie']
                });
            });
        });

        req.on('error', reject);
        if (options.body) {
            req.write(options.body);
        }
        req.end();
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
        this.solvedChallenges = new Set();
        this.availableChallenges = [];
    }

    log(message) {
        console.log(`[Player ${this.id}/${this.username}] ${message}`);
    }

    async fetchStaticFiles() {
        try {
            // Fetch main page
            await httpRequest(`${SERVER_URL}/`);
            this.log('Fetched index.html');

            // Fetch CSS
            await httpRequest(`${SERVER_URL}/css/ctfg.css`);
            this.log('Fetched CSS');

            // Fetch banner image
            await httpRequest(`${SERVER_URL}/images/banner.png`);
            this.log('Fetched banner image');

            // Fetch JavaScript
            await httpRequest(`${SERVER_URL}/js/app.js`);
            this.log('Fetched app.js');
        } catch (err) {
            this.log(`Error fetching static files: ${err.message}`);
        }
    }

    async login() {
        try {
            this.log(`Attempting login with username: ${this.username}, password: ${this.password.substring(0,3)}...`);

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

            if (response.status === 200) {
                this.sessionCookie = response.cookies ? response.cookies[0] : null;
                const data = JSON.parse(response.data);
                this.log(`Logged in successfully. Needs name: ${data.needs_name}`);

                // Extract WebSocket URL
                const wsUrl = data.websocket_url || `ws://localhost:12345`;
                this.wsUrl = wsUrl;
                this.log(`WebSocket URL from server: ${wsUrl}`);

                return true;
            } else {
                this.log(`Login failed with status ${response.status}: ${response.data}`);
                return false;
            }
        } catch (err) {
            this.log(`Login error: ${err.message}`);
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
        return new Promise((resolve) => {
            // Check if wsUrl already ends with /scorestream
            const wsUrl = this.wsUrl.endsWith('/scorestream')
                ? this.wsUrl
                : `${this.wsUrl}/scorestream`;
            this.log(`Connecting to WebSocket: ${wsUrl}`);

            this.ws = new WebSocket(wsUrl);

            this.ws.on('open', () => {
                this.log('WebSocket connected');
                this.wsConnected = true;

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
                this.log(`WebSocket error: ${err.message}`);
                this.wsConnected = false;
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

            const data = JSON.parse(response.data);
            if (response.status === 200) {
                if (data.result === 'correct') {
                    this.log(`✓ Challenge ${challengeId} solved! (+${data.points} points, total: ${data.total})`);
                    this.solvedChallenges.add(challengeId);

                    // Emulate browser: reload challenges after successful submission
                    await this.getChallenges();

                    return true;
                } else if (data.result === 'already_solved') {
                    this.log(`Challenge ${challengeId} already solved`);
                    this.solvedChallenges.add(challengeId);
                    return true;
                } else {
                    this.log(`✗ Challenge ${challengeId} incorrect flag`);
                    return false;
                }
            } else {
                this.log(`Submit failed with status ${response.status}: ${response.data}`);
                return false;
            }
        } catch (err) {
            this.log(`Submit error: ${err.message}`);
            return false;
        }
    }

    async solveChallenges() {
        let solvedCount = 0;
        let maxIterations = 50; // Prevent infinite loops
        let consecutiveFailures = 0;

        // Get initial challenges
        let availableChallenges = await this.getChallenges();

        while (maxIterations-- > 0) {
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

            const results = await Promise.all(submissions);
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

            // Minimal delay between batches (worst case stress testing)
            await sleep(100);
        }

        this.log(`Completed solving challenges. Total solved: ${solvedCount}`);
    }

    async run() {
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
        await Promise.all([
            this.setDisplayName(),
            this.connectWebSocket()
        ]);

        // Minimal delay before starting to solve (worst case)
        await sleep(200);

        // Start solving challenges
        await this.solveChallenges();

        // Keep WebSocket alive for a bit
        this.log('Keeping connection alive...');
        await sleep(10000);

        // Cleanup
        if (this.ws && this.wsConnected) {
            this.ws.close();
        }

        this.log('Player simulation complete');
    }
}

async function main() {
    console.log(`Starting ${NUM_PLAYERS} player emulation(s) against ${SERVER_URL}`);
    console.log('----------------------------------------');

    const players = [];

    // Create player instances
    for (let i = 0; i < NUM_PLAYERS; i++) {
        const cred = credentials[i];
        const player = new Player(i + 1, cred.username, cred.password);
        players.push(player);
    }

    // Run players concurrently - WORST CASE: all start at exactly the same time
    const promises = players.map(async (player, index) => {
        // NO STAGGERING - true thundering herd for stress testing
        return player.run();
    });

    // Wait for all players to complete
    await Promise.all(promises);

    console.log('----------------------------------------');
    console.log('All player simulations complete');
}

// Run the main function
main().catch(err => {
    console.error('Fatal error:', err);
    process.exit(1);
});