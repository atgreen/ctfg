# CTFG Resilience and Availability Review

## Executive Summary
- Overall readiness: Strong foundation for a 200‑player/3‑hour event, with WAL‑backed SQLite, CAS‑based in‑memory caches, static asset caching, and a WebSocket scorestream. However, several security and operational gaps (WS origin policy, static path traversal, missing DB indexes, prod backtraces) must be addressed to ensure resilience under load.
- Top risks:
  - WebSocket origin/auth open — data exposure to unauthorized observers.
  - Static path traversal in custom dispatcher — unintended file disclosure.
  - Missing SQLite indexes on hot queries — latency spikes and contention.
  - Production backtraces enabled — sensitive information leakage.

## Architecture Snapshot
- Backend: routes, session, rate limiting
  - Easy‑Routes/Hunchentoot app; taskmaster sized to 500 threads. Evidence: `src/server.lisp:745–753`.
  - Sessions via `hunchentoot:start-session`; user bound in `:user`. Evidence: `src/server.lisp:239–245, 253–261`.
  - Rate limiting with token buckets + RW locks applied to submit, hint, set‑name. Evidence: `src/rate-limiter.lisp:12–23, 64–79`; `src/server.lisp:318–321, 336–339, 419–423`.
- WebSocket: registration, backlog, replay
  - Resource `/scorestream` registered with `any-origin`. Evidence: `src/server.lisp:607–613`.
  - Server threads: `ws:run-server 12345` and `run-resource-listener`. Evidence: `src/server.lisp:716–743`.
  - Backlog default 5 in CLWS; tuned write backlog in app. Evidence: `ocicl/clws-20240503-b20799d/server.lisp:96–106`; `src/server.lisp:707–711`.
  - Historical replay as one JSON array per connect. Evidence: `src/server.lisp:614–637`.
- Database: schema, write paths, atomicity
  - SQLite with WAL, NORMAL sync, busy_timeout; per‑thread connection cache; transactional `BEGIN IMMEDIATE` flows for atomicity. Evidence: `src/db.lisp:134–144, 191–231, 251–312`.
  - Schema lacks indexes. Evidence: `src/db.lisp:150–164`.
- Concurrency: locks, CAS
  - Custom RW lock with writer priority. Evidence: `src/rwlock.lisp:28–66`.
  - CAS solves table using luckless lists. Evidence: `src/server.lisp:16, 35–47`.
- Static Assets: cache, headers, path handling
  - In‑memory static cache with dev/production cache headers; custom dispatcher. Evidence: `src/server.lisp:80–116, 149–188`.
- Front End
  - SPA with robust WS reconnect, backoff, ping; batch replay handling. Evidence: `js/app.js:516–599, 600–629, 570–578`.

## Findings

### Critical
- WebSocket origin/auth open (data exposure)
  - Evidence: `src/server.lisp:610–612`
    "(ws:register-global-resource "/scorestream" … #'ws::any-origin)"
  - Rationale: Any origin may connect to ws://host:12345/scorestream; no token/session validation on the WS layer.
  - Impact: Unauthorized parties can observe live scores and display names.
  - Recommendation: Restrict origins (exact/prefix) and require a signed token on WS URL; validate in `resource-accept-connection`.
  - Verification: Block foreign‑origin connects and missing/invalid tokens; allow valid ones; exercise 200 client reconnect.

- Static path traversal in custom dispatcher
  - Evidence: `src/server.lisp:149–161, 163–188`
    Uses `(subseq script-name 1)` merged with root without sanitization; serves if `probe-file` succeeds.
  - Rationale: Requests like `/js/../index.html` pass prefix test and can traverse to unintended files.
  - Impact: Disclosure of files outside intended static roots.
  - Recommendation: Normalize paths and reject `..` components, or use `hunchentoot:handle-static-file`/built‑in static handler with a fixed root and prefix.
  - Verification: `/js/../index.html` and encoded variants return 404.

### High
- Missing DB indexes on hot paths
  - Evidence: `src/db.lisp:191–227, 265–306, 350–360, 150–164` — frequent predicates over `(user_id, challenge_id, event_type)` and `MAX(hint_number)`; schema lacks indexes.
  - Rationale: Full scans under concurrency raise latency and extend lock holds.
  - Impact: Slower submissions/hint purchases; increased timeout risk.
  - Recommendation: Add indexes:
    - `CREATE INDEX IF NOT EXISTS events_user_chal_submit ON events(user_id, challenge_id, event_type);`
    - `CREATE INDEX IF NOT EXISTS events_user_chal_hint ON events(user_id, challenge_id, event_type, hint_number);`
    - `CREATE INDEX IF NOT EXISTS events_ts ON events(ts);`
  - Verification: Query plans use indexes; emulator shows lower p95 latency.

- Production backtraces enabled
  - Evidence: `src/server.lisp:674–676`
    Sets `*show-lisp-errors-p*` and `*show-lisp-backtraces-p*` to T.
  - Rationale: Error pages leak internals in production.
  - Impact: Information disclosure.
  - Recommendation: Set to NIL unless `*developer-mode*` is true; keep Sentry reporting.
  - Verification: Trigger errors; responses lack backtraces.

- CLWS accept backlog too small for bursts
  - Evidence: `ocicl/clws-20240503-b20799d/server.lisp:96–106` — `:backlog 5`.
  - Rationale: Herd connects will overflow backlog and drop connections.
  - Impact: Reconnect failures and degraded UX.
  - Recommendation: Patch to increase backlog (256–1024) for event build.
  - Verification: Simulate herd connects; observe accept success rate.

- WAL checkpoint control missing
  - Evidence: `src/db.lisp:134–144` sets WAL; no auto‑checkpoint or periodic checkpoint elsewhere.
  - Rationale: WAL can grow unbounded over 3 hours with steady writes.
  - Impact: Disk growth; potential write stalls.
  - Recommendation: `PRAGMA wal_autocheckpoint=1000;` at init and/or scheduled `PRAGMA wal_checkpoint(TRUNCATE);` during lulls.
  - Verification: WAL file bounded across emulator run.

### Medium
- `/api/award` logs token and lacks rate limit
  - Evidence (logging): `src/server.lisp:388–391`.
  - Evidence (no rate limit): `src/server.lisp:386–415` (no `check-rate-limit`).
  - Rationale: Logging secrets is unsafe; unbounded writes/broadcasts risk DoS if token is abused.
  - Impact: Credential leakage; elevated load.
  - Recommendation: Remove/obfuscate token logs; apply `*api-rate-limiter*` with a service key.
  - Verification: Logs omit token; repeated calls hit 429.

- Unbounded WS history replay size
  - Evidence: `src/server.lisp:614–637` streams entire history as one array; `js/app.js:570–578` processes batch.
  - Rationale: Payload grows over event; slower connects and transient stalls.
  - Impact: Late joiners experience lag; resource spikes.
  - Recommendation: Bound replay window (e.g., last N minutes) or move history fetch to paged HTTP; WS for live deltas only.
  - Verification: Late joins complete quickly; CPU and WS frame sizes bounded.

### Low
- Mixed `bt`/`bt2` threading APIs
  - Evidence: `src/rwlock.lisp:22–27` (`bt:` wait/notify; `bt2:` broadcast); ocicl v2 exports `condition-broadcast` (v1 does not). Evidence: `ocicl/bordeaux-threads-0.9.4/apiv2/pkgdcl.lisp:4–5, 71`; `apiv1/pkgdcl.lisp`.
  - Rationale: Intentional to access v2 feature; may confuse maintainers.
  - Impact: Low; documentation/optional unification recommended.
  - Recommendation: Add a short code comment or migrate to bt2 consistently.

## Performance & Scalability
- DB query/index analysis
  - Hot read-before-write checks (`SELECT 1 FROM events WHERE user_id=? AND challenge_id=? AND event_type=1`) and `MAX(hint_number)` query require composite indexes to avoid scans. Evidence: `src/db.lisp:201–210, 276–284`.
  - WAL + NORMAL sync reduce writer stall; busy_timeout 30s has headroom. Evidence: `src/db.lisp:134–144`.
- WebSocket connect/replay strategy
  - Single-array replay amortizes rendering on client. Evidence: `src/server.lisp:618–634`; client handles batch with `flushChart`. Evidence: `js/app.js:572–578`.
  - Increase CLWS `:backlog` and keep `ws::*max-write-backlog*` reasonable (app sets a very high bound; consider 256–512) — `src/server.lisp:707–711`; ocicl default backlog: `server.lisp:96–106`.
- Emulator expectations
  - With static caching and indexes, 200 players sustained. Watch first WS burst and DB contention during synchronized submits; ensure index coverage and rate limits.

## Security & Operational Hardening
- Error handling: turn off backtrace in prod; keep Sentry capture hooks. Evidence: `src/server.lisp:667–676` for Sentry capture hook.
- Origin policy: switch from `any-origin` to exact/prefix; add signed token binding session user.
- Path traversal: sanitize inputs or rely on Hunchentoot static serving with explicit root.
- Secrets: remove token logs; `.env` already supported. Evidence: `src/main.lisp:128–139, 84`.
- WAL management: add auto‑checkpoint; monitor `events.db-wal` size.

## Implementation Plan (Prioritized)
- [M] WS: restrict origin; add signed token check in `resource-accept-connection` — Blocks unauthorized stream access.
- [S] Static: sanitize/replace custom dispatcher with safe static file serving — Prevents traversal.
- [S] Errors: disable backtraces in prod/non‑dev — Reduces info leaks.
- [M] DB: add three indexes — Lowers latency; reduces contention.
- [M] CLWS: increase accept backlog — Improves resilience to reconnect storms.
- [S] WAL: add `wal_autocheckpoint` — Caps WAL growth.
- [S] `/api/award`: remove token logging; apply `*api-rate-limiter*` — Prevents credential leaks/abuse.
- [M] WS replay bound (time window or paged HTTP) — Lowers connect spike.
- [S] Docs: comment bt/bt2 rationale; optional unify — Maintainer clarity.
- [S] Optional: server heartbeat to reap zombies — Extra resilience.

## Verification Plan
- Run emulator at 10, 70, 200 users (jitter 10s; and 0s herd). Collect mean/median/p95/p99 latencies and errors.
- Confirm static traversal blocked via targeted requests.
- Observe SQL `EXPLAIN QUERY PLAN` pre/post indexes; inspect lock/wait metrics if available.
- Induce WS reconnect storm; ensure high accept rate with increased backlog and stable memory.
- Monitor `events.db-wal` growth; confirm checkpoints.
- Validate 429s and absence of token logs on `/api/award`.
- Confirm WS token/origin rejection path and acceptance for valid tokens.

## References
- Source:
  - `src/server.lisp:149–188, 239–261, 607–613, 614–637, 667–676, 674–676, 707–711, 716–743, 745–753`
  - `src/db.lisp:134–144, 150–164, 191–231, 251–312, 350–360`
  - `src/rwlock.lisp:22–27, 28–66`; `src/clients.lisp:22–41`
  - `js/app.js:516–599, 600–629, 570–578`
  - `src/main.lisp:84, 128–139`
- Library (ocicl):
  - `ocicl/clws-20240503-b20799d/server.lisp:96–106`; `resource.lisp:177–205, 229–239`
  - `ocicl/bordeaux-threads-0.9.4/apiv2/pkgdcl.lisp:4–5, 71`; `apiv2/api-condition-variables.lisp:90–97`; `apiv1/pkgdcl.lisp`
- External:
  - SQLite WAL Mode — https://www.sqlite.org/wal.html
  - SQLite Query Planning/Indexes — https://www.sqlite.org/optoverview.html

