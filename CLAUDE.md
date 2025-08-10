# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

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

### Cleaning
```bash
make clean      # Remove build artifacts and test files
```

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