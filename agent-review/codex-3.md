CTFG Resilience and Availability Review (Codex 3)

Round 2 feedback digested. Below I lock down disputed points with concrete evidence, expand on the websocket replay question, and integrate additional hardening ideas.

Disputed Point: award-points-atomic increment
- Position: The increment executes unconditionally. This is observable in the source and in the API response logic.
- Evidence (src/server.lisp lines 296–312):
  - `(when success ...)` spans lines 296–310; the `let`, `log:info`, `save-solve`, and `dolist` are inside this block.
  - Line 311 contains `(incf (user-total-points user) (challenge-points challenge))` at the same indentation level as the `when`, and after the `when`’s closing paren on 310.
  - Line 312 returns `success` from `multiple-value-bind`.
- Behavioral implication: On a duplicate flag submit, `record-flag-if-not-solved` returns NIL, so no event is inserted/broadcast, but `user-total-points` is still incremented. The HTTP response for duplicate (`:result "already_solved" :total <points>`) will show an inflated total, and subsequent hint affordability checks may be incorrect.
- Resolution: Move the `incf` into the `when success` body.

WebSocket Replay: strategy and client impact
- Current behavior: On connect, server spawns a thread and sends the full `events` table as a single array. Under heavy history this is costly.
- Client expectations: The frontend accepts both a batch (array) and live single events. It uses the batch to reconstruct timelines and a scoreboard map.
- Safer replay options (compatible with current client):
  - Windowed replay: send only the last K events (e.g., by `ORDER BY ts DESC LIMIT K`, then reverse on send). For a 3‑hour event, K≈10k is usually sufficient.
  - Snapshot + delta: compute current per‑user totals server‑side and send an initial “score snapshot” message, then follow with recent events. This avoids full history but would require a small client change to accept the snapshot.
  - Chunking: If retaining full history, chunk into multiple frames (e.g., blocks of 1k messages) to reduce memory spikes and allow progressive rendering.

Static file serving: additional hardening
- I agree with the suggestions to:
  - Serve static assets from a dedicated domain/subdomain with strict CSP.
  - Prefer a reverse proxy (e.g., nginx) for static files in production, keeping Hunchentoot for dynamic routes only.
  - Keep the in‑process static cache as a fallback or for dev mode.

Small refinements and confirmations
- Origin policy: Whitelist known event origins to reduce abuse surface with minimal operational burden.
- Memory and thread lifecycle: Headroom is acceptable; shutdown paths already destroy the cleanup thread. Leave as is.
- Indexes: Retain the three recommended indexes; they directly accelerate the identified queries.
- `/api/award` robustness: Use `ensure-user` or return 4xx; correct `user-solved-p` argument.
- Regex flags: Prefer exact compare; if regex needed, escape and anchor the stored value.

Updated prioritized actions (no change in order)
1) Fix `/api/hint` parameter key.
2) Remove sensitive logging.
3) Disable detailed error pages in production.
4) Move the `incf` into the `when success` body in `award-points-atomic`.
5) Harden static path resolution; reject traversal; consider proxying static.
6) Add and analyze indexes.
7) Compute hint affordability inside the transaction.
8) WS backlog raise + replay windowing/chunking; optionally snapshot totals.
9) `/api/award` resilience and `user-solved-p` fix.
10) Exact/anchored flag check.

Rationale summary for reviewers
- I grounded contested claims in concrete paren matching and line numbers to avoid indentation ambiguity.
- WS replay changes remain compatible with the current client; a future snapshot message can further reduce replay costs.
- Production static hardening via proxy/CSP adds defense‑in‑depth with minimal code change.

