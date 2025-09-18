# CTFG Resilience and Availability Review

Scope: Common Lisp backend (Hunchentoot + Easy‑Routes + CLWS), SQLite event store, custom concurrency primitives, and SPA frontend. Objective: support ~200 concurrent players for ~3 hours with low latency and high reliability.

## Executive Summary

The architecture is solid for a single‑node CTF engine: WAL‑tuned SQLite for event sourcing, immutable event log semantics, custom RW locks and CAS lists for in‑memory state, and a WebSocket scorestream. Static asset caching is thoughtfully implemented. With several correctness and security fixes, plus small performance optimizations, the system should comfortably handle the target load.

## Strengths

- Database
  - WAL mode, sensible pragmas (busy_timeout, NORMAL sync, in‑RAM temp/cache, mmap), microsecond timestamps.
  - Atomic operations implemented with `BEGIN IMMEDIATE` and a clear separation between cached and fresh connections.

- Concurrency
  - Custom reader‑writer lock with writer priority for client list operations; lock‑free CAS list for per‑user solve tracking.
  - Per‑client write serialization before `ws:write-to-client-text` to prevent socket races.

- Real‑time and caching
  - CLWS resource model used cleanly; write backlog tuned; initial event hydration on connect.
  - In‑memory static asset cache with 1‑year cache headers in production and no‑cache in developer mode; preload of common assets.

- Abuse control
  - Token‑bucket rate limiters per user+endpoint with background cleanup to control memory usage.

## Critical Issues (fix before event)

1) Hint API parameter mismatch breaks hints
   - Server reads `:hint--id` while client posts `hint_id`; all hint purchases fail (HTTP 400).
   - Fix: Align server to accept `:hint_id`.

2) Sensitive data logged
   - `/api/submit` logs flag guesses; `/api/award` logs the Authorization token.
   - Fix: Remove/obfuscate sensitive values; log metadata only (user ID, challenge ID, outcome).

3) Detailed error pages enabled in production
   - `*show-lisp-errors-p*` and `*show-lisp-backtraces-p*` set to true; leaks internals and increases error payloads.
   - Fix: Set both to NIL outside developer mode; retain Sentry exception capture.

4) Points increment bug in `award-points-atomic`
   - `(incf (user-total-points user) ...)` executes outside `(when success ...)` and runs on duplicate submissions.
   - Impact: Inflated totals in API responses and incorrect hint affordability; scoreboard remains inconsistent with DB truth.
   - Fix: Move the increment into the `when success` block; add a regression test for duplicate submissions.

5) Static file traversal risk
   - Custom dispatcher merges `script-name` with CWD relying on prefix checks; `..` segments could escape intended subtrees.
   - Fix: Reject `..`/`~` segments; normalize paths; serve only from whitelisted roots (or proxy static content).

## Performance and Scalability

6) WebSocket backlog and history replay
   - CLWS listener backlog is 5; connection storms (start/reconnect) can cause refusals and retry amplification.
   - `send-events` replays full history per connect in one large frame; grows over time and spikes CPU/GC/bandwidth.
   - Fixes:
     - Raise backlog in CLWS (vendor patch) and ensure write‑backlog sizing matches peak bursts.
     - Use windowed replay (e.g., last K events via `ORDER BY ts DESC LIMIT K`, reversed before send) and/or chunk the replay in manageable batches.
     - Optional: Send an initial “score snapshot” (current per‑user totals) and then recent deltas.

7) Database indexing
   - Hot queries: solve dedupe `(user_id, challenge_id, event_type)`, hint checks `(… hint_number)`, event ordering `ORDER BY ts`.
   - Add indexes:
     - `CREATE INDEX IF NOT EXISTS idx_events_user_chal_type ON events(user_id, challenge_id, event_type);`
     - `CREATE INDEX IF NOT EXISTS idx_events_ts ON events(ts);`
     - Optional: `idx_events_hint_seq ON events(user_id, challenge_id, event_type, hint_number)`.
   - Run `ANALYZE` after warmup to guide the planner.

8) Transactional hint affordability check
   - Current code relies on an in‑memory `current-points` value; can be stale or wrong under bugs.
   - Fix: In the same `BEGIN IMMEDIATE` transaction, compute `SUM(points)` and verify affordability before insert.

## Correctness and Security Hardening

9) `/api/award` robustness and misuse of `user-solved-p`
   - Unknown usernames result in NIL user; later dereferences can error.
   - `user-solved-p` expects `(user-id challenge-id)` but receives a user object; the precheck is wrong (DB still dedupes).
   - Fix: Use `(ensure-user *db* username)` or return 4xx; correct the `user-solved-p` call or remove it.

10) Flag matching safety
   - Using unanchored regex matching for flags can lead to false positives or regex backtracking cost.
   - Fix: Prefer exact string comparison; if regex is required, escape and anchor the stored value.

11) WebSocket origin policy
   - `any-origin` is operationally convenient but invites embedding/abuse load.
   - Fix: Whitelist known event origins in production.

12) Duplicated globals
   - `*challenges-path*`/`*websocket-url*` defined in multiple units; avoid load‑order surprises.
   - Fix: Single source of truth for globals.

## Operational Readiness

- Sessions: Long‑lived sessions are acceptable for a time‑boxed event; keep GC frequency elevated.
- Observability: Add counters (rate‑limit rejects, duplicate submissions, hint errors, WS connect/disconnect, replay size). Keep Sentry with appropriate sampling.
- Static content: Prefer a reverse proxy for `/css`, `/js`, `/images` with CSP; keep in‑process cache for dev/fallback.

## Prioritized Action Plan

1. Fix hint parameter key (`:hint_id`).
2. Remove sensitive logging (flags, Authorization tokens).
3. Disable detailed error pages in production; keep Sentry capture.
4. Fix `award-points-atomic` to increment only on success; add a duplicate‑submission regression test.
5. Harden static file dispatcher; consider proxying static with CSP.
6. Add event table indexes and run `ANALYZE` after warmup.
7. Move hint affordability to the DB transaction (authoritative total).
8. WebSocket: raise backlog, window and/or chunk replay; optionally send score snapshots.
9. `/api/award` robustness and `user-solved-p` correction/removal.
10. Exact/anchored flag comparison.

## Expected Outcome

With the critical fixes and modest performance tweaks above, CTFG should reliably serve 200 concurrent players for 3 hours with accurate scoring, resilient real‑time updates, and a hardened security posture. The proposed changes are surgical, maintain client compatibility, and provide a pragmatic path to a stable event‑day deployment.

