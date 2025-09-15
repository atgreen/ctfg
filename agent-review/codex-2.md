# CTFG Resilience and Availability Review — codex-2

## Change Log
- Clarified S-expression analysis for `award-points-atomic` to reflect correct body forms within the `when` form.
- Accepted new critical finding on `bt2` package usage and `%broadcast` typo; added evidence and priority adjustments.
- Accepted WAL checkpointing recommendation and expanded operational steps.
- Contested claim about unbounded WS client registry with evidence of disconnect handling.
- Strengthened `/api/award` rate limit recommendation and token logging fix with explicit evidence.

## Accepted vs. Contested Points

### Accepted
- Critical: `bt2` package and `%broadcast` typo
  - Evidence: src/rwlock.lisp:26
    "(defun %broadcast (cv) (bt2:condition-broadcast cv))"
  - Evidence: src/package.lisp:7–11
    "(defpackage #:ctfg
      (:use #:cl #:bt2)
      …)"
  - Evidence: src/rate-limiter.lisp:104–111, 127–132
    uses `bt2:make-thread`, `bt2:thread-alive-p`, `bt2:destroy-thread` for cleanup thread.
  - Rationale/Impact: `bt2` is not a standard package in ocicl; compilation/runtime would fail. This is a P0 blocker for running from source; must be corrected to `bt:` (Bordeaux Threads) or appropriate package.
  - Recommendation: Replace `bt2:` with `bt:` throughout (`rwlock.lisp`, `rate-limiter.lisp`, package `:use`). Verify that `bt:` exposes `condition-broadcast` or provide an alternative (e.g., emulate broadcast via looped `condition-notify`).
  - Verification: Build from source; run smoke tests and ensure rate-limit cleanup thread starts; exercise RW lock paths.

- High: WAL checkpoint control
  - Evidence: src/db.lisp:134–144 enables WAL and performance PRAGMAs; no `wal_checkpoint` or `wal_autocheckpoint` present elsewhere.
  - Rationale: During a 3‑hour event with steady writes, WAL may grow; without auto-checkpointing, disk usage can creep and fsyncs bunch.
  - Recommendation: Add `PRAGMA wal_autocheckpoint = 1000;` at init and/or scheduled `PRAGMA wal_checkpoint(TRUNCATE);` during quiet periods.
  - Verification: Observe `events.db-wal` size over time during emulator runs.

- Medium: `/api/award` logs bearer token and lacks rate limiting
  - Evidence (logging): src/server.lisp:388–391 — `(log:info access-token)`.
  - Evidence (no RL): src/server.lisp:386–415 contains no `check-rate-limit` call; compare submit/hint/set-name at src/server.lisp:318–321, 336–339, 419–423.
  - Recommendation: Remove token logging (or log hash/first 8 chars). Apply `check-rate-limit *api-rate-limiter*` keyed to a service user or IP.
  - Verification: Confirm header not logged; hammer endpoint and receive 429s.

- High: CLWS accept backlog of 5
  - Evidence (ocicl): ocicl/clws-20240503-b20799d/server.lisp:96–106 — `:backlog 5` in `with-open-socket`.
  - Recommendation: Patch to increase backlog (e.g., 256–1024) for event; contribute upstream or vendor patch locally.

- S-expression correction (non-substantive outcome change)
  - Evidence: src/server.lisp:296–312 shows `when success` body consisting of two forms: a `let` containing `(log:info …)`, `(save-solve …)`, `(dolist …)`; and a sibling `(incf …)`.
  - Conclusion remains: broadcast, cache update, and point increment only occur on success.

### Contested
- High: “Unbounded WebSocket Client Registry” and no cleanup
  - Claim: Clients are registered without cleanup, causing memory leak.
  - Evidence of cleanup: src/server.lisp:657–663
    "(defmethod ws:resource-client-disconnected ((resource scorestream-resource) client)
      … (remove-client client) …)"
  - Evidence of removal: src/clients.lisp:29–39
    "(defun remove-client (client) … (setf *websocket-clients* (delete client *websocket-clients* :key #'client-socket :test #'equal)) …)"
  - Assessment: The code removes clients on disconnect; no leak by design. Monitoring is still advised to detect edge cases (e.g., abrupt socket drops without callback), but severity should be lowered or dropped.

## Clarified S-expression Analysis

Focus: `award-points-atomic`

- Structural outline (raw): src/server.lisp:293–312
  "(log:info …)
   (multiple-value-bind (success ts event-id) …
     (when success
       (let ((msg …))
         (log:info msg)
         (save-solve …)
         (dolist (…) (with-write-lock-held (…) (ws:write-to-client-text …))))
       (incf (user-total-points user) (challenge-points challenge)))
     success)"
- Body of `when`: exactly two forms, in order:
  1) a `let` containing three body forms (log/save/broadcast),
  2) the `(incf …)` form.
- The `success` symbol is outside `when` but inside the `multiple-value-bind` body, which is inside the defun.

## Updated Priorities
1) P0: Fix `bt2` usages and `%broadcast` typo (blocking for source builds).
2) P1: Secure WS origin/token, fix static traversal, disable prod backtraces.
3) P2: Add DB indexes; increase CLWS backlog; add WAL auto-checkpoint.
4) P3: Rate-limit `/api/award`; remove token logging; bound WS replay window.

