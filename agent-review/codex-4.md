CTFG Resilience and Availability Review (Codex 4)

Focused follow-up on the contested `award-points-atomic` behavior and final refinements prior to the final review.

award-points-atomic: resolving the disagreement
- Core question: Is `(incf (user-total-points user) (challenge-points challenge))` guarded by `(when success ...)`?
- Direct reading (src/server.lisp 296–312):
  - 296: `(when success`
  - 297–305: `let` establishes `msg` and closes at 305
  - 306–310: `log/save-solve/dolist` execute (still within `when`)
  - 311: `(incf (user-total-points user) (challenge-points challenge))`
  - 312: `success))`

Two possibilities exist syntactically:
  A) The `when` body contains both the `let` block and the `incf`, i.e., `(when success (let ...) (incf ...))`.
  B) The `incf` is outside `when`, i.e., `(when success (let ...)) (incf ...)`.

Why B is the actual behavior here:
  - The final `)` that closes the `when` appears before the `incf`. This is visible from the paren closure at the end of the `dolist` line (310) followed by a new top‑level form at 311. The closing `success))` at 312 belongs to the surrounding `multiple-value-bind` and `defun`.
  - Practical effect check (no REPL needed): perform a duplicate correct submission. The response path for duplicates ends in:
    - `submit` route: returns `(:result "already_solved" :total ,(user-total-points user))` when `award-points-atomic` returns NIL.
    - If the `incf` were conditional, `:total` would remain unchanged on a duplicate. If unconditional, `:total` will increase despite `:result "already_solved"` and no broadcast event. This is the current, incorrect behavior.

Recommendation: Move the `incf` inside the `when success` and add a regression test that submits the same valid flag twice and asserts no change in `:total` on the second response.

WebSocket replay: implementation guidance
- Start with windowed replay: `SELECT ... ORDER BY ts DESC LIMIT K`, reverse before send. Choose K conservatively (e.g., 10,000) given expected volume (solves + hints).
- If time permits, add an optional “score snapshot” message that provides per‑user totals to seed the client scoreboard without requiring full history.
- Chunk long replays in batches (e.g., 1k events) to avoid GC spikes.

Static file serving: deployment guidance
- Route `/css`, `/js`, `/images` via reverse proxy in production; keep Hunchentoot static handler only for dev.
- Add a CSP appropriate for the SPA and tighten allowed sources.
- Retain the in‑process cache as a performance fallback if the proxy is unavailable.

Final prioritized actions (unchanged order)
1) Fix `/api/hint` parameter key.
2) Remove sensitive logging.
3) Disable detailed error pages in production.
4) Guard the `incf` with `when success`.
5) Harden static path resolution; consider proxy/CSP.
6) Add indexes and analyze.
7) Transactional affordability check for hints.
8) WS backlog + replay windowing/chunking; optional snapshot.
9) `/api/award` robustness and `user-solved-p` arg fix.
10) Exact/anchored flag check.

