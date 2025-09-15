CTFG Resilience and Availability Review (Codex 2)

Incorporating reviewer feedback from Claude and Gemini. I confirm where we agree, and I challenge points where evidence contradicts their assertions. I include rationale so you can audit each claim quickly.

What I Accepted (agreement)
- Hint API mismatch is game‑breaking. Server reads `:hint--id` while client posts `hint_id` → all hint purchases fail with 400. Fix server key to `:hint_id`.
- Logging of sensitive data must be removed. Do not log flag guesses or the Authorization token.
- Disable detailed error responses in production. Keep Sentry capture; set `*show-lisp-errors-p*` and `*show-lisp-backtraces-p*` to NIL when not in developer mode.
- Static file dispatcher needs traversal hardening. Prefix checks help, but add explicit rejection of `..`/`~` segments and serve only from whitelisted roots.
- WebSocket backlog of 5 is too low for 200 players and reconnect storms. Raise backlog in CLWS (vendor patch) and chunk/window event replay.
- Add event table indexes aligned to hot queries: `(user_id, challenge_id, event_type)`, `ts`, and optionally `(user_id, challenge_id, event_type, hint_number)`.
- Move hint affordability check fully into the DB transaction to avoid races and stale in‑memory totals.

Where I Disagree (with rationale)
1) award-points-atomic points increment placement
   - Gemini believes the `(incf (user-total-points user) ...)` is inside the `(when success ...)` block. It is not. The parens close the `when` before the `incf`, so `incf` runs unconditionally.
   - Evidence (structure simplified):
     (multiple-value-bind (success ts event-id) ...
       (when success
         ;; build and broadcast msg ...)
       (incf (user-total-points user) (challenge-points challenge))
       success)
   - Outcome: points are incremented even when `record-flag-if-not-solved` returns NIL (already solved). Fix: move `incf` into the `when success` block.

2) Origin policy “may be intentional” for CTF
   - Even if scoreboard data is non‑sensitive, an open origin check enables trivial embedding that can amplify load or be used for nuisance traffic. In practice, origin checks are cheap to restrict to your event domain(s) without hampering participants. Recommendation stands: whitelist expected origins in production.

Additional Clarifications (based on reviewer asks)
- Memory usage of in‑memory solve tracking
  - `*solves-table*` maps `user-id` → luckless CAS list of challenge IDs. Upper bound for the event is modest: 200 players × ≤30 challenges ≈ ≤6,000 nodes. This is acceptable headroom.
  - Persisted truth lives in SQLite; `*solves-table*` is a fast cache hydrated on boot from `collect-events`.

- Thread lifecycle
  - Rate‑limit cleanup thread: created once, destroyed in `shutdown-server` via `stop-rate-limit-cleanup` (calls `bt2:destroy-thread`). WebSocket server and resource listener threads are started with handlers that capture and log exceptions. This is reasonable for a single‑node event; just ensure `shutdown-server` is called during controlled stops.

- Database index benefit
  - Observed statements:
    - Solve dedupe: `WHERE user_id=? AND challenge_id=? AND event_type=1 LIMIT 1`
    - Hint checks: `SELECT 1 … user_id=? AND challenge_id=? AND hint_number=? AND event_type=2`, plus `MAX(hint_number)` with same prefix.
    - Event replay: `ORDER BY ts`.
  - Proposed indexes directly support these filters/sorts, reducing lock contention and tail latencies under bursty loads.

Confirmed Bugs/Weaknesses with Evidence
- `/api/award` robustness
  - Uses cache lookup `(lh:gethash username *username-to-user-object*)`; NIL user can flow to later derefs. Fix: use `(ensure-user *db* username)` or 4xx on unknown usernames.

- `user-solved-p` misused in `/api/award`
  - Signature is `(user-id challenge-id)`; code passes the user object and `cid`. The check is wrong (though dedupe is ultimately enforced by the DB). Fix: pass `(user-id user)` or drop the redundant precheck.

- Regex flag matching
  - Current path: `ppcre:count-matches (challenge-flag chal) guess`. This treats the stored flag as a regex and isn’t anchored. To avoid both false positives and regex backtracking surprises, use exact comparison or anchor/escape the stored flag.

- Duplicated globals
  - `*challenges-path*` and `*websocket-url*` are defined in multiple compilation units. Prefer a single definition to avoid load‑order surprises.

Refined Action Plan (with rationale)
1) Correct `/api/hint` parameter key to `:hint_id` and keep 4xx mapping for other errors.
2) Remove/obfuscate sensitive logging (flags and Authorization token). Log metadata only.
3) Disable detailed error pages in production mode; keep Sentry exception capture.
4) Fix `award-points-atomic` so `incf` executes only on success.
5) Harden static file dispatcher to reject traversal and serve only known subtrees.
6) Add DB indexes; run `ANALYZE` after first warmup to help the planner.
7) Move hint affordability check to run inside `BEGIN IMMEDIATE` with `SUM(points)` to enforce correctness.
8) WebSocket improvements: raise backlog (vendor patch), chunk/window event replay, and ensure write‑backlog limits are sized for peak bursts.
9) `/api/award` robustness: `ensure-user` or return 4xx; fix `user-solved-p` call.
10) Exact/anchored flag comparison to avoid regex pitfalls.

Notes on Operational Readiness
- Sessions are configured to persist; appropriate for a 3‑hour event with manual lifecycle.
- Observability: consider counters (rate‑limit rejects, dup‑submit, hint‑locked, WS connects/disconnects) for quick triage during the event.

Conclusion
Both reviewers validate the most important risks and agree on the highest‑priority fixes. The only contested point—the unconditional points increment—is a real bug by inspection and should be corrected. With these fixes and small scalability adjustments, the system should comfortably meet the event’s resilience and availability targets.

