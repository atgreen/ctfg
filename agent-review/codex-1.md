CTFG Resilience and Availability Review (Codex 1)

Scope: Common Lisp backend (Hunchentoot + Easy‑Routes + CLWS), SQLite event store, custom concurrency primitives, and SPA frontend. Goals: sustain ~200 concurrent players for ~3 hours with real‑time scoring.

Summary
- Overall architecture is sound for a single‑node event engine. WAL‑tuned SQLite, immutable event log, per‑client WS locks, and static asset caching are good foundations.
- A few correctness and security issues pose immediate risk (hint API bug, unsafe logging, error disclosure, minor race/logic bugs). Fix these before the event.
- Throughput headroom is likely fine, but replaying full event history to each reconnecting client can create spikes; consider windowing/chunking.

Key Strengths
- Database: WAL mode, NORMAL sync, busy_timeout, memory pragmas; atomic writes via BEGIN IMMEDIATE; explicit helper macros for fresh vs cached connections (src/db.lisp).
- Concurrency: Reader‑writer lock for client list; luckless CAS structures for solve lists; per‑socket write lock before ws:write (src/clients.lisp, src/server.lisp, ocicl/luckless-*).
- Real‑time: CLWS resource model with dedicated listener; write‑backlog raised; initial scoreboard hydration on WebSocket connect (src/server.lisp).
- Static assets: In‑memory cache with 1‑year cache headers in prod and dev no‑cache; preload common assets on boot (src/server.lisp).
- Rate limiting: Token bucket per user+endpoint, background cleanup to avoid unbounded growth (src/rate-limiter.lisp).

High‑Risk Issues (fix first)
- Hint purchase API parameter mismatch breaks hints
  - Server expects `:hint--id` (double hyphen) but frontend sends `hint_id` (js/app.js → POST /api/hint). Result: 400 missing parameters; hints unusable.
  - Fix: Read `:hint_id` on the server.

- Sensitive data exposure in logs
  - `/api/submit` logs the raw `guess` (flag) and `/api/award` logs the Authorization token.
  - Impact: Secret leakage; log bloat under brute‑force; operational risk.
  - Fix: Remove/obfuscate sensitive values; log only user/challenge IDs and outcome.

- Production error disclosure enabled
  - `start-server` enables `*show-lisp-errors-p*` and `*show-lisp-backtraces-p*` while `*catch-errors-p*` is true.
  - Impact: Leaks internals; big error bodies during failure.
  - Fix: Set both to NIL unless in developer mode; keep Sentry reporting.

- Incorrect points increment on duplicate submission
  - `award-points-atomic` increments `user-total-points` outside the success branch, so it increments even when already solved. This desynchronizes in‑memory points and may allow unintended hint purchases.
  - Fix: Move the `incf` inside the success branch.

- Path handling in static file dispatcher
  - `cached-static-dispatcher` trusts `script-name` and merges path; prefixes are restricted to `/css/`, `/js/`, `/images/`, but `..` segments can still traverse to other files when resolved by `probe-file` (e.g., `/images/../events.db`).
  - Fix: Reject any path containing `..` or `~`, normalize, and/or serve only from a fixed chrooted asset mapping via pathname directory components.

Medium‑Risk / Scalability
- WebSocket backlog and history replay
  - CLWS listener backlog is 5 (ocicl/clws/server.lisp); with 200 clients, thundering‑herd connects can see reset/refusal and cause retry storms. Also, `send-events` pushes full history as a single array per client; grows over time and may exceed client limits or cause GC pressure.
  - Fixes: Increase backlog in CLWS (or patch vendor) and consider chunking/windowing event replay (e.g., last K events or last N minutes) with multiple frames.

- DB indexes
  - Hot paths query by `(user_id, challenge_id, event_type)` and `MAX(hint_number)`, plus `ORDER BY ts` reading all events.
  - Add: `CREATE INDEX IF NOT EXISTS idx_events_user_chal_type ON events(user_id, challenge_id, event_type);`
    `CREATE INDEX IF NOT EXISTS idx_events_ts ON events(ts);`
    Optional: `idx_events_hint_seq ON events(user_id, challenge_id, event_type, hint_number)`.

- Affordability check for hints uses in‑memory points argument
  - `record-hint-atomic` receives `current-points` from the user object; if the in‑memory counter is stale (or incorrect due to the bug above), affordability may be misjudged.
  - Improvement: Compute points inside the same transaction (`SUM(points) WHERE user_id=?`) to make it authoritative even under future multi‑process scale‑out.

- Origin policy overly permissive
  - WS resource registered with `any-origin`; acceptable for pure scoreboard, but consider an origin predicate in production to reduce ambient load.

Correctness / Robustness
- `/api/award` robustness for unknown users
  - Uses cache lookup for user; NIL user would cause errors on deref. Use `(ensure-user *db* username)` to create/fetch.

- `user-solved-p` misuse in `/api/award`
  - Called with a user object instead of `user-id`; the atomic DB guard prevents double award, but the precheck is wrong. Pass `(user-id user)` or remove the redundant check.

- Regex flag matching can cause false positives and ReDoS
  - `ppcre:count-matches` against `challenge-flag` treats the flag as a regex and does not anchor; crafted flags or inputs could cause unintended matches or backtracking cost.
  - Fix: Use constant‑time equality for exact flags, or compile an anchored literal (`^...$`) with `:simple` to avoid pathological patterns.

- Duplicated globals
  - `*websocket-url*` and `*challenges-path*` are defparameter’ed in multiple files; harmless but noisy. Prefer single definition.

- Hardcoded obfuscation key
  - `mask-string` uses a static key; for simple obfuscation it’s fine, but avoid implying secrecy; document that it’s not cryptographic.

Operational Readiness
- Sessions: `*session-max-time*` to `most-positive-fixnum` is fine for a 3‑hour event; keep `*session-gc-frequency*` elevated. Consider a max age if deployment lingers post‑event.
- Observability: Sentry integration is present; consider sampling to limit volume under storms. Add counters for reject reasons (rate limit, already_solved, auth_fail) and WS connect/disconnect metrics.
- Load testing: Use provided emulator with jitter to validate WS replay changes and ensure static cache preloads are effective.

Actionable Fix List (prioritized)
1) Fix hint param key; align server with `hint_id`.
2) Stop logging flags and Authorization tokens.
3) Disable detailed error pages outside developer mode; keep Sentry capture.
4) Fix `award-points-atomic` increment placement.
5) Harden static path handling against traversal.
6) Add DB indexes for event queries; vacuum/analyze once after initial warmup.
7) Move hint affordability check fully into the transaction.
8) Limit and chunk WS event replay; raise CLWS backlog and write‑backlog as needed.
9) Ensure user exists in `/api/award`; fix `user-solved-p` call.
10) Make flag check exact or anchored; avoid regex DoS.

Notes on Library Semantics (verified in ocicl)
- CLWS backlog defaults to 5 in server acceptor. `*max-write-backlog*` is tunable; backlog is not without patching.
- Luckless CAS list’s `member` returns a boolean; suitable for `user-solved-p` checks once the correct key is used.
- Hunchentoot header names are case‑insensitive; `header-in :AUTHORIZATION` is valid.

Closing
With the correctness and security fixes plus small DB and WS refinements, this codebase should sustain the target load with reliable behavior. The biggest availability risks for the event are the hint API bug, error/log leakage, static file traversal, and full‑history WS replay during reconnect storms—address these first.

