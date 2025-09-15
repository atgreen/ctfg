# CTFG Resilience and Availability Review — codex-4

## Change Log
- Incorporated round-3 confirmations: bt2 availability verified; no further disputes.
- Added note explaining why bt2 is used (v1 lacks condition-broadcast) with ocicl v1/v2 citations.
- Finalized priorities and verification steps; aligned with critics on WS cleanup robustness and optional heartbeats.

## Accepted vs. Contested Points

### Accepted
- bt2 usage is intentional to access `condition-broadcast` missing from bt (v1).
  - Evidence (v2 define/export): ocicl/bordeaux-threads-0.9.4/apiv2/pkgdcl.lisp:4–5, 71; api-condition-variables.lisp:90–97.
  - Evidence (v1 exports lack broadcast): ocicl/bordeaux-threads-0.9.4/apiv1/pkgdcl.lisp (no condition-broadcast export).
  - Recommendation: Keep mixed usage or fully migrate to bt2; add a short comment in rwlock.lisp noting rationale.
- WS disconnect handling robust per CLWS; residual zombie risk addressed via optional server heartbeat.
  - Evidence: ocicl/clws-20240503-b20799d/resource.lisp:229–239; src/server.lisp:657–663; src/clients.lisp:29–39.

### Contested
- None. All parties aligned.

## Final Priorities
- P1 Security: WS origin/token; static traversal; disable prod backtraces.
- P2 Perf/Ops: Add DB indexes; raise CLWS backlog; WAL auto-checkpoint; rate-limit `/api/award`.
- P3 Hardening: Remove token logging; document/unify bt/bt2; optional server heartbeat; bound WS replay window.

## Verification (Converged)
- WS policy/token: reject foreign origins/no-token; accept signed tokens; 200-client reconnect storm.
- Static traversal: `/js/../index.html` returns 404.
- DB: EXPLAIN QUERY PLAN shows index usage; emulator shows stable P95 latencies.
- CLWS: Backlog set high; successful accepts under herd.
- WAL: `events.db-wal` bounded; periodic checkpoint runs.
- Award RL: 429s after token-bucket depletion; no token logging.
- bt2: `(find-symbol "CONDITION-BROADCAST" :bt2)` resolves and works.

