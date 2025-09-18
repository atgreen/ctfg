# AGENTS.md

This file provides guidance to AI agents and developers when working with code in this repository.

## Architecture Overview

CTFG is a Capture-The-Flag game engine with a Common Lisp backend and modern web frontend. The system follows a client-server architecture with real-time WebSocket communication for live scoring updates.

### Backend (Common Lisp)
- **Web Framework**: Hunchentoot with Easy-Routes for HTTP routing
- **WebSocket**: CLWS for real-time scorestream
- **Database**: SQLite3 via DBI for persistent event storage
- **Concurrency**: Custom reader-writer locks (src/rwlock.lisp) for thread-safe operations
- **Entry Point**: src/main.lisp (CLI and server initialization)

### Frontend
- **Single-Page Application**: index.html with js/app.js
- **Styling**: Tailwind CSS
- **Real-time Updates**: WebSocket connection to /scorestream endpoint
- **Visualization**: Chart.js for score progression

### Key Components

1. **Challenge System** (src/challenges.lisp)
   - Loads from challenges.json with hot-reload support in dev mode
   - Supports dependency chains via requirements field
   - Hint system with progressive point deduction

2. **Authentication** (src/user.lisp)
   - Simple CSV-based credentials (credentials.csv)
   - Session management with display name customization

3. **Event Processing** (src/event.lisp)
   - All game events stored in SQLite database
   - Real-time broadcast to WebSocket clients

4. **External API** (src/server.lisp)
   - POST /api/award for automated flag submission
   - Requires CTFG_API_TOKEN environment variable

## Project Structure & Module Organization
- Root: `ctfg.asd` (ASDF system), `Makefile`, `index.html`, `challenges.json`, `credentials.csv`, `game-clusters.yaml`.
- Source (Common Lisp): `src/` — files loaded via `ctfg.asd` in declared order. Add new modules here and update `:components` in `ctfg.asd`.
- Frontend assets: `css/ctfg.css`, `js/app.js`, images in `images/`. Asset tarball `runtime-files.tgz` is generated; do not hand‑edit.
- Tests: `tests/` — Playwright specs are generated from `tests/make_tests.sh` using `tests/credentials.csv`.
- Artifacts: `ctfg` (executable), `events.db*` (SQLite), `playwright-report/`.

## Common Development Commands

### Building
```bash
make            # Build the ctfg executable
```

### Running
```bash
./ctfg --help   # Show all available options
./ctfg -p 9090  # Run on port 9090
./ctfg -d       # Run in developer mode (hot-reload challenges.json)
```

### Testing
```bash
make check      # Generate and run 50 concurrent user tests with Playwright
```

### Load Testing
```bash
# Test with N concurrent players using the included emulator
./player-emulator.js <server_url> challenges.json credentials.csv <N> [jitter_seconds]

# Examples:
./player-emulator.js http://localhost:8080 challenges.json credentials.csv 70 10    # 10s jitter (realistic)
./player-emulator.js http://localhost:8080 challenges.json credentials.csv 70 0     # thundering herd (stress test)
./player-emulator.js http://localhost:8080 challenges.json credentials.csv 5 2      # quick 2s burst

# The emulator generates:
# - metrics-<timestamp>.json: Raw performance data
# - performance-report-<timestamp>.html: Interactive performance report with charts
# - Individual static file timing breakdown (CSS, JS, images)
# - Response time statistics (mean, median, percentiles, std dev)
```

### Cleaning
```bash
make clean      # Remove build artifacts and test files
```

## Important Configuration Files

- **challenges.json**: Challenge definitions with flags, points, and dependencies
- **credentials.csv**: Player authentication (username,password format)
- **game-clusters.yaml**: Kubernetes cluster configuration for distributed deployment
- **events.db**: SQLite database (auto-created on first run)

## Development Tips

- Use `--developer-mode` flag to enable hot-reloading of challenges.json
- WebSocket URL must be configured correctly for production deployments (--websocket-url option)
- Test generation script creates Playwright tests: `./tests/make_tests.sh <number_of_users>`
- All static files are embedded in the binary via runtime-files.tgz during build

## Recent Backend Updates (Important for Agents)

### SQLite Usage
- Connections are no longer cached per thread. Every DB block opens a fresh connection and closes it (see `with-open-connection`/`with-fresh-connection` in `src/db.lisp`). This eliminates FD leaks and stale handles.
- Submits are idempotent: `record-flag-if-not-solved` uses a single `INSERT OR IGNORE` guarded by a unique partial index. No explicit transaction or long writer lock is taken for submits.
- Hints remain transactional; `BEGIN IMMEDIATE` has a bounded retry to tolerate transient `SQLITE_BUSY`.
- Startup creates helpful indexes and enforces uniqueness:
  - `uniq_submit(user_id, challenge_id) WHERE event_type = 1`
  - `uniq_hint(user_id, challenge_id, hint_number) WHERE event_type = 2`
  Duplicate legacy rows are cleaned before index creation.
- To avoid `SQLITE_CANTOPEN` bursts, concurrent connects are capped via a small lock+CV gate (default 128). Tweak `*sqlite-max-connections*` in `src/db.lisp` if needed.
- A short retry on `CANTOPEN` exists around `sqlite:connect` for resilience; `busy_timeout` is set to 30s for queries.

Practical tips:
- If you change the DB location, use `-b/--dbdir`; ensure the directory exists and is writable. Most test failures with `CANTOPEN` are path/permissions or FD limit issues.
- Avoid reintroducing connection pools/caches unless you also add strict lifecycle cleanup.

### WebSocket (CLWS)
- The opening handshake now uses RFC-compliant CRLF line endings on all status/header lines. This fixes Node `ws`/`wscat` errors like “Missing expected CR after response line”. The change lives in the local `~/git/clws` repo:
  - `protocol-7.lisp` (101 Switching Protocols), `protocol.lisp` (400), `config.lisp` (400/403/404), `server.lisp` (503).
- Server keepalive pings are enabled by default; `*ws-keepalive-interval*` and `*ws-ping-timeout*` control cadence and timeout. PONGs update `client-last-pong-ts` in `src/server.lisp`.
- Load/backlog: the CLWS listener can accept a `:backlog` (see `*ws-backlog*`); increase for large bursts.

Testing tips:
- Validate the handshake with `wscat -c ws://127.0.0.1:12345/scorestream`. If it fails, confirm you restarted after CLWS changes and you are connecting to IPv4.
- From curl: send an Upgrade request and expect `101 Switching Protocols`.

### Player Emulator
- Normalizes ws URL to IPv4 (`127.0.0.1`) and disables permessage-deflate to avoid negotiation issues.
- Explicitly responds to protocol-level PING with PONG, reducing `ECONNRESET` from missed keepalive.
- Use a non-zero jitter for realistic behavior; `0` creates a thundering herd.

## Troubleshooting Quick Reference
- WebSocket parse error (CR expected): ensure you’re on the CRLF-enabled CLWS and connecting to `ws://127.0.0.1:12345/scorestream` (or the TLS front door with a proper proxy). Test with `wscat`.
- ECONNRESET on ws: indicates peer closed; check keepalive PING/PONG handling and `*ws-ping-timeout*`. Emulator now auto-PONGs.
- SQLITE_CANTOPEN: typically FD burst or invalid path. Fresh connections + gate + short retry are in place. Verify `--dbdir` and permissions; raise `*sqlite-max-connections*` only if necessary.

## Performance Optimization

### Static File Caching
CTFG implements in-memory caching for static files (CSS, JS, images) to improve performance under high load:

- **Production mode**: Aggressive caching with 1-year browser cache headers
- **Developer mode**: Caching disabled for hot-reloading during development
- **Preloading**: Common files cached at server startup
- **Thread-safe**: Uses locks for concurrent access protection
- **Memory efficient**: ~374KB for typical static assets

Cache benefits: Response times drop from ~50ms (disk I/O) to <1ms (memory) for cached files.

### Load Testing Best Practices
- Start with small player counts (5-10) to establish baseline performance
- Use jitter (10+ seconds) for realistic user behavior simulation
- Use thundering herd (0 jitter) for worst-case stress testing
- Monitor static file breakdown to identify bottlenecks (large images, slow JS)
- Check server logs for "Static cache preloaded" messages to verify caching is active

## Coding Style & Naming Conventions
- Common Lisp: 2‑space indentation; package is `ctfg`. Use kebab‑case for symbols (e.g., `save-solve`), docstrings on public defs, align keyword params, avoid global state unless needed. One feature per file; file names `kebab-case.lisp`.
- JavaScript (`js/app.js`): 2 spaces, semicolons, strict mode. Prefer small pure helpers; reuse existing `SecurityUtils` and `UIUtils`. Avoid direct HTML injection; sanitize input.
- Assets: keep Tailwind utility classes in HTML; edit `css/ctfg.css` directly when needed.

## Testing Guidelines
- Framework: Playwright (`playwright.config.ts` starts `./ctfg -d` on port 8080).
- Spec naming: `tests/ctfg-<username>.spec.ts` (generated). Keep custom specs under `tests/` with `.spec.ts` suffix.
- Data: ensure `tests/credentials.csv` exists; tests derive users from it.
- Reports: HTML report in `playwright-report/`.

## Commit & Pull Request Guidelines
- Commits: short, imperative subject (e.g., "Fix hint purchase error"). Group related changes; include brief body if behavior changes.
- PRs: include purpose, key changes, how to test (`make check` steps), and screenshots for UI updates. Link issues. Avoid committing secrets or `events.db*`.

## Security & Configuration Tips
- Use `.env` for secrets (e.g., `CTFG_API_TOKEN`); never commit it.
- DB path is configurable via `-b/--dbdir`. WebSocket URL via `-w/--websocket-url`.
- Reset local state by deleting `events.db*` (Playwright does this automatically).
