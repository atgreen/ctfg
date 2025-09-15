# CTFG Resilience and Availability Review

## Executive Summary
- Overall readiness: solid foundation for a 200‑player/3‑hour CTF with good primitives (SQLite in WAL, CAS lists, WS batching, static caching). However, several production hardening gaps could impact resilience and security.
- Top risks:
  - WebSocket origin/auth is open; scoreboard stream is publicly readable via any origin (data exposure). Evidence: src/server.lisp:610–612 “any-origin”.
  - Static file dispatcher permits “..” traversal in paths under `/js/`, `/css/`, `/images/` (file disclosure risk). Evidence: src/server.lisp:149–188 path handling.
  - SQLite lacks critical indexes; SELECT checks run full scans during peak, risking latency spikes. Evidence: src/db.lisp:191–227, 276–306; no indexes created.
  - Hunchentoot debug/backtraces enabled in production; leaks stacktraces on errors. Evidence: src/server.lisp:674–676.
  - WebSocket accept backlog is 5 in clws; thundering herd can drop connects during mass reconnects. Evidence: ocicl/clws-…/server.lisp:96–106.

## Architecture Snapshot
- Backend: routes, session, rate limiting
  - Easy‑Routes over Hunchentoot; one‑thread‑per‑connection taskmaster sized for 500 threads. Evidence: src/server.lisp:745–753.
  - Session: `hunchentoot:start-session` and session-value `:user`. Evidence: src/server.lisp:239–245, 253–261.
  - Rate limiting: token buckets with custom RW locks; applied to `/api/submit`, `/api/hint`, `/api/set-name`. Evidence: src/rate-limiter.lisp:12–23, 64–79; src/server.lisp:318–321, 336–339, 419–423.
- WebSocket: registration, backlog, replay logic
  - Resource registered at `/scorestream`, origin policy `any-origin`. Evidence: src/server.lisp:607–613.
  - Server threads: `ws:run-server 12345` and resource listener threads. Evidence: src/server.lisp:716–743.
  - Backlog limit tuned via `ws::*max-write-backlog*`. Evidence: src/server.lisp:707–711; default is 16 in ocicl. Evidence: ocicl/clws-…/client.lisp:3–4.
  - Replay: on connect, builds one JSON array of all historical events. Evidence: src/server.lisp:614–637.
- Database: schema, write paths, atomicity
  - SQLite with WAL, NORMAL sync, busy_timeout, per‑thread connection cache; transactional `BEGIN IMMEDIATE` for atomic operations. Evidence: src/db.lisp:134–144, 191–231, 251–312.
  - Events table schema (no indexes). Evidence: src/db.lisp:150–164.
- Concurrency: locks, CAS structures
  - Custom RW lock (writer priority). Evidence: src/rwlock.lisp:28–66.
  - Solve cache using luckless CAS lists keyed by user. Evidence: src/server.lisp:16, 35–47; used in award paths.
- Static Assets: cache, headers, path handling
  - In‑memory cache with 1‑year browser caching; disabled in dev. Evidence: src/server.lisp:80–116, 149–188, 190–199.
- Front End: javascript analysis
  - Robust WS reconnection with exponential backoff, ping, size guard; batch processing for replay. Evidence: js/app.js:516–547, 554–599, 600–629, 562–578.

## Findings

### Critical
- WebSocket Origin/Auth Open (Data Exposure)
  - Evidence: src/server.lisp:610–612
    "(ws:register-global-resource "/scorestream"
     (make-instance 'scorestream-resource)
     #'ws::any-origin)"
  - Rationale: With `any-origin` and no session binding on WS connections, anyone can connect to `ws://host:12345/scorestream` and read live scoring. For public events this may be acceptable, but it violates least privilege and leaks participant display names and timing.
  - Impact: Unauthorized observers can scrape score stream; in some environments this is a privacy/competitive risk.
  - Recommendation: Restrict origins and require an auth token on the WS URL that the HTTP session issues.
    - Change origin policy to exact-or-prefix matching tied to the served origin.
    - Require a signed query param (HMAC over user id + expiry) and validate in `resource-accept-connection`.
  - Verification: Attempt to connect from a non‑origin page and without token; expect reject. Connect with valid token; expect success. Load test 200 reconnects.

- Static Path Traversal in Cached Dispatcher
  - Evidence: src/server.lisp:149–188
    "(subseq script-name 1) ; Remove leading slash … merge-pathnames … if (probe-file file-path) … return content"
  - Rationale: The dispatcher accepts any path beginning with `/css/`, `/js/`, or `/images/` and then merges `script-name` as a relative pathname. Inputs like `/js/../index.html` still pass the prefix check and, with Common Lisp pathname semantics, can traverse upward. `probe-file` then resolves and serves unintended files.
  - Impact: Read‑only disclosure of `index.html` or other repo files not intended under those prefixes.
  - Recommendation: Normalize and reject any component containing `..` or absolute paths before `merge-pathnames`; alternatively, use `hunchentoot:static-file` with `:prefix` and `:root` and let it handle traversal.
  - Verification: Request `/js/../index.html` and `/css/%2e%2e/index.html`; expect 404 after fix.

### High
- Missing DB Indexes on Hot Paths
  - Evidence: src/db.lisp:191–227 (flag check/insert), 276–306 (hint queries), 350–360 (event scan), 150–164 (schema missing indexes)
  - Rationale: Queries filter by `(user_id, challenge_id, event_type)` and aggregate `MAX(hint_number)` with those predicates. Without indexes, each submission/hint purchase scans the `events` table, amplifying latency under concurrency.
  - Impact: Latency spikes and increased transaction hold time; contention and timeouts at 200 players during bursts.
  - Recommendation: Add indexes:
    - `CREATE INDEX IF NOT EXISTS events_user_chal_submit ON events(user_id, challenge_id, event_type);`
    - `CREATE INDEX IF NOT EXISTS events_user_chal_hint ON events(user_id, challenge_id, event_type, hint_number);`
    - `CREATE INDEX IF NOT EXISTS events_ts ON events(ts);`
  - Verification: Explain query plans before/after; measure submission latency under emulator (with WAL active).

- Debug Errors/Backtraces Enabled in Production
  - Evidence: src/server.lisp:674–676
    "(setf hunchentoot:*catch-errors-p* t)
     (setf hunchentoot:*show-lisp-errors-p* t)
     (setf hunchentoot:*show-lisp-backtraces-p* t)"
  - Rationale: Enabling error displays can leak internals in HTTP responses, increasing information exposure surface.
  - Impact: Sensitive info disclosure during failures.
  - Recommendation: Set both `*show-lisp-errors-p*` and `*show-lisp-backtraces-p*` to NIL unless `*developer-mode*` is true.
  - Verification: Force a handler error and confirm generic 500 without backtrace.

- WebSocket Accept Backlog Too Small for Herds
  - Evidence (library): ocicl/clws-20240503-b20799d/server.lisp:96–106
    "… :backlog 5 …"
  - Evidence (usage): src/server.lisp:724–728 starts `ws:run-server` without overriding backlog.
  - Rationale: A backlog of 5 can drop incoming connections when many clients reconnect simultaneously (browser retry storms or network blips).
  - Impact: Reconnect storms fail, degrading UX and scoreboard freshness.
  - Recommendation: Expose and increase backlog in `clws` `run-server` or patch locally to accept a higher backlog (e.g., 256–1024) for the event.
  - Verification: Simulate 200 clients connecting simultaneously; observe accept rate and server log.

### Medium
- `/api/award` Logs Authorization Token
  - Evidence: src/server.lisp:388–396
    "(log:info access-token) … compare with *ctfg-api-token*"
  - Rationale: Logging bearer tokens/secrets risks credential leakage via logs.
  - Impact: Token exposure to operators/log systems; potential abuse.
  - Recommendation: Do not log token; if needed, log hashed/first 8 chars only.
  - Verification: Submit with header and check logs.

- `/api/award` Unbounded Rate
  - Evidence: No `check-rate-limit` on award route; compare to submit/hint/set-name routes.
  - Rationale: Even with token, a script can hammer writes/broadcasts and cause contention.
  - Impact: Elevated DB load and WS spam if token leaks.
  - Recommendation: Apply `*api-rate-limiter*` to `/api/award` keyed by a service principal.
  - Verification: Hammer endpoint; expect 429 after N requests.

- WebSocket History Replay Size Grows Unbounded
  - Evidence: src/server.lisp:614–637 – builds a single array of every event each connection.
  - Rationale: As events grow (hints + solves), initial payload can get large; string concatenation in Lisp and WS frame may cause latency for late joiners.
  - Impact: Slower connects, potential per‑client stalls under load.
  - Recommendation: Limit replay window (e.g., recent 1–2 hours) or paginate via HTTP `/api/score-history?since=…` and WS only for live updates.
  - Verification: Connect late after generating thousands of events; measure first message latency.

### Low
- Fixed Secret in `mask-string`
  - Evidence: src/server.lisp:547–549 – uses "some-secret-key".
  - Rationale: Not security‑critical, but avoid hardcoded strings.
  - Impact: Low; obfuscation only.
  - Recommendation: Derive key from `CTFG_SECRET` env, rotate between events.

- Regex Flags Matching
  - Evidence: src/server.lisp:434–445 – `ppcre:count-matches (challenge-flag chal) guess`.
  - Rationale: Regex mis‑configs could match partials; ensure anchors.
  - Impact: Accepting near matches if flags lack anchors.
  - Recommendation: Ensure flags are anchored or escaped per challenge policy.

## Performance & Scalability
- DB paths and indexes
  - Events writes are single‑row INSERTs; WAL + NORMAL sync + 30s busy timeout. Evidence: src/db.lisp:134–144, 170–189, 191–231, 251–312. Indexes are missing; add as above.
- WebSocket connect/replay
  - Batch array on first connect is good for render amortization. Evidence: src/server.lisp:618–634; client processes arrays efficiently. Evidence: js/app.js:570–578.
- CLWS buffering and backlog
  - Increase `ws::*max-write-backlog*` wisely: default 16 (ocicl/clws-…/client.lisp:3–4). Current tuning sets it to `(* 500 30)` per comment; consider a moderate cap (e.g., 256–512) plus drop policy.
- Emulator expectations
  - With static cache preload and WAL, the app should sustain 200 players with staggered traffic. Watch for contention during first WS connect bursts and for path traversal checks.

## Security & Operational Hardening
- Error handling/backtraces
  - Set debug toggles off unless in dev. Evidence: src/server.lisp:674–676.
- Origins and CSRF
  - Restrict WS origin and add token; for HTTP, consider SameSite cookies (Hunchentoot default is cookie‑based sessions). Apply `hunchentoot:no-cache` to sensitive API responses as needed.
- Static traversal
  - Sanitize paths; or revert to built‑in static handler.
- Secrets
  - Stop logging tokens; ensure `.env` is used (already supported). Evidence: src/main.lisp:128–139; src/main.lisp:84 binds `CTFG_API_TOKEN`.

## S‑expression Analysis

1) Award Points (Atomic) broadcast occurs only on success

Raw snippet: src/server.lisp:293–312
"(log:info "award points atomic")
 (multiple-value-bind (success ts event-id)
     (record-flag-if-not-solved *db* user challenge)
   (when success
     (let ((msg (format nil …)))
       (log:info msg)
       (save-solve (user-id user) (challenge-id challenge))
       (dolist (client (get-client-list))
         (with-write-lock-held ((client-lock client))
           (ws:write-to-client-text (client-socket client) msg)))
       (incf (user-total-points user) (challenge-points challenge))))
   success)"

Structural outline:
"(defun award-points-atomic
  (log:info …)
  (multiple-value-bind (success ts event-id)
    (record-flag-if-not-solved …)
    (when success
      (let ((msg …)) …
           (log:info …)
           (save-solve …)
           (dolist (…) (with-write-lock-held (…) (ws:write-to-client-text …)))
           (incf …)))
    success))"

- The target `when` form body has exactly four forms, in order: `(let …)`, `(log:info …)`, `(save-solve …)`, `(dolist …)`, `(incf …)`; the `success` return is outside `when` but inside the `multiple-value-bind` body, and the whole `defun` ends after that.
- Conclusion: Broadcast, cache update and point increment only happen when `success` is truthy.

2) Static dispatcher path test

Raw snippet: src/server.lisp:149–161
"(let* ((script-name (hunchentoot:script-name request))
        (static-paths '("/css/" "/js/" "/images/")))
  (when (some (lambda (prefix) (alexandria:starts-with-subseq prefix script-name)) static-paths)
    (lambda ()
      (let* ((file-path (merge-pathnames (subseq script-name 1) (app-root)))) …)))"

Structural outline:
"(defun cached-static-dispatcher (request)
  (let* ((script-name …)
         (static-paths …))
    (when (some … static-paths)
      (lambda () (let* ((file-path (merge-pathnames (subseq script-name 1) (app-root)))) …)))))"

- The `when` body is a single form returning a lambda; inside, `file-path` is constructed directly from `script-name` substring. There is no sanitization or normalization eliminating `..`.
- Conclusion: Inputs like `/js/../index.html` enter the lambda and are merged unfiltered.

## Implementation Plan (Prioritized)
- [M] Restrict WS origin and add token auth for `/scorestream`.
- [S] Sanitize static paths or revert to hunchentoot static handler to prevent traversal.
- [S] Disable backtraces/errors in prod when not `*developer-mode*`.
- [S] Stop logging `/api/award` token; add rate limit to `/api/award`.
- [M] Add SQLite indexes for event queries and ts ordering.
- [M] Raise clws accept backlog (patch or forked parameter) and set a sane `*max-write-backlog*` (e.g., 256–512).
- [M] Bound WS history replay window; move older history to paged HTTP fetch.

## Verification Plan
- Security:
  - WS: connect from foreign origin and without token → reject; valid token → accept.
  - Static traversal: request `/js/../index.html` → 404.
- Performance:
  - Run emulator at N=70 with jitter 10s, then N=200 jitter 0; measure mean/percentiles and error rates. Confirm no DB timeouts.
  - Observe “Static cache preloaded” logs and low latencies on static fetch.
- Stability:
  - Kill WS resource listener; confirm restart path or crash logging; reconnect behavior within 30s.

## References
- Source:
  - src/server.lisp:607–613, 614–637, 667–676, 691–701, 707–715, 716–743, 745–753
  - src/server.lisp:149–188, 386–396, 414–445
  - src/db.lisp:134–144, 150–164, 170–189, 191–231, 251–312, 350–360
  - src/rwlock.lisp:28–66; src/clients.lisp:22–41
  - js/app.js:516–547, 554–599, 600–629, 570–578
  - ocicl/clws-20240503-b20799d/client.lisp:3–4; ocicl/clws-20240503-b20799d/server.lisp:96–106; ocicl/clws-20240503-b20799d/resource.lisp:21–33, 47, 177–205
  - ocicl/easy-routes-20250522-4bc58ca/easy-routes.lisp:67–83
- External:
  - SQLite WAL Mode — https://www.sqlite.org/wal.html
  - SQLite Query Planning/Indexes — https://www.sqlite.org/optoverview.html

