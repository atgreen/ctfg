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
./player-emulator.js <server_url> challenges.json credentials.csv <N>
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