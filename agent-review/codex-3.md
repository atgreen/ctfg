# CTFG Resilience and Availability Review — codex-3

## Change Log
- Corrected my prior P0 conclusion regarding `bt2`; confirmed availability in ocicl bordeaux-threads v2 and demoted from blocker.
- Added ocicl citations for `bt2` package and `condition-broadcast` symbol.
- Incorporated nuance on WebSocket client cleanup and proposed server-driven heartbeat for resilience.
- Kept priorities for security and performance unchanged; clarified maintenance note on mixed threading APIs.

## Accepted vs. Contested Points (Round 2)

### Accepted (Corrections)
- `bt2` is available and intended
  - Evidence: ocicl/bordeaux-threads-0.9.4/apiv2/pkgdcl.lisp:4–5, 71
    "(defpackage :bt2 (:nicknames :bordeaux-threads-2)) … exports … #:condition-broadcast"
  - Evidence: ocicl/bordeaux-threads-0.9.4/apiv2/api-condition-variables.lisp:90–97 defines `condition-broadcast`.
  - Evidence: src/rwlock.lisp:22–27 uses `bt:` for wait/notify and `bt2:condition-broadcast` for broadcast.
  - Conclusion: Mixed API usage appears deliberate; not a build blocker. Demote previous P0 to Low/Docs: clarify rationale or consolidate to one API for maintainability.

- WS client cleanup exists; edge cases acknowledged
  - Evidence: src/server.lisp:657–663 and src/clients.lisp:29–39 implement disconnect removal.
  - Library behavior: ocicl/clws-20240503-b20799d/resource.lisp:177–205 routes EOF/DROPPED to `disconnect-client`.
  - Refinement: Zombie connections may persist under network partitions absent keepalives; suggest server‑initiated ping/idle reap.

### Contested (None material)
- No material disagreements remain in round 2; severity/priorities updated accordingly.

## Priority Adjustments
- P1 (Critical Security):
  1) WS origin/token
  2) Static traversal
  3) Disable prod backtraces
- P2 (High Perf/Operational):
  4) Add DB indexes
  5) Increase CLWS accept backlog
  6) WAL auto-checkpoint
  7) Rate-limit `/api/award`
- P3 (Medium/Hardening):
  8) Remove token logging
  9) Document or unify bt/bt2 usage
  10) Optional: server heartbeat to reap stale WS clients

## Verification Additions
- `bt2` availability: confirm `(find-symbol "CONDITION-BROADCAST" :bt2)` resolves; sanity broadcast on a test CV.
- WS liveness: implement periodic server `write-to-client-text` ping or clws ping frame if supported; drop unresponsive clients and verify list shrinks.

