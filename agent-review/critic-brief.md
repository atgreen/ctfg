# Critic Brief: CTFG Resilience and Availability Review

You are an expert software architect and programmer skilled in many languages and tools. You are reviewing the application in this directory, which is a game engine intended to support 200 players for a 3‑hour intense event.
You are a Critic.  You will provide expert analysis and critique of software Reviewer.

## Reviewer's Objectives
- Evaluate resilience, availability, scalability and UX under the 200‑player/3‑hour constraint.
- Identify correctness, security, and operational risks that would impact the event.
- Provide actionable fixes with rationale, impact, and verification steps.
- Cross‑check claims against local library sources in `ocicl` rather than relying on memory.

## Scope
- In‑scope: Common Lisp backend (Hunchentoot, Easy‑Routes, CLWS), SQLite usage, concurrency primitives, SPA/WebSocket behavior, static assets pathing/caching, rate limiting.
- Out‑of‑scope: Long‑term features beyond event readiness.

## Evidence Protocol
- Always cite exact code with file path and line range.
  - Use the format: `src/server.lisp:296–312` plus a short, verbatim snippet when making structural claims.
  - Prefer numbered snippets (e.g., via `nl -ba`) when possible.
- For any claim about library semantics or defaults, cite the `ocicl` source path and line range (e.g., `ocicl/clws-YYYY…/server.lisp:104`).
- For external operational guidance (e.g., SQLite WAL), include a short reference in a References section (title + URL). Keep external references limited to well-known docs.

## S-expression Analysis Protocol
- Do not rely on indentation; reason from parentheses.
  - Identify which closing parenthesis ends each of: the target form (e.g., `when`/`let`/`dolist`), the enclosing form (e.g., `multiple-value-bind`), and the `defun`.
- Enumerate body forms explicitly.
  - For any claim like “X is inside the `when`,” list the forms in the body of that `when` in order.
- Provide a structural outline before concluding.
  - Example (abbreviated):
```lisp
(defun foo
 (multiple-value-bind (...)
   (when success
     (let ((msg ...)) ...)
       (incf ...))
    success))
```
- If ambiguity remains, pretty‑print the snippet and re‑outline.
  - Show both pretty‑printed and raw versions.
- Macro awareness: `when` can have multiple body forms; verify by listing them.
- Only assert “inside/outside” after this process; include the evidence inline.

## Deliverables and Iteration
- Read `agent-review/codex-1.md` once it exists (wait via shell loop). Form your own analysis; do not accept assertions without evidence. Confirm library behavior by reading `ocicl` code where applicable.
- For any structured claim (e.g., “form X is inside `when`”), follow the S‑expression Analysis Protocol. If you disagree, include your structural outline and the exact paren boundary lines.
- Respond to each `codex-N.md` with `agent-review/claude-N.md` or `agent-review/gemini-N.md`, depending on who you are.
  - Organize your response:
    - Summary of agreements
    - Contested points (each with your evidence and structural outline when relevant)
    - Additional findings with evidence
    - Priority adjustments (if any)
- Keep critiques focused on resilience, availability, correctness, and security for the event window.
- Your last critique will be number 4.  So you will go through 4 iterations of this.

### Shell Wait Loop Snippets
- Wait for a single file (e.g., codex-1.md or a critique):
  ```bash
  #!/usr/bin/env bash
  set -euo pipefail
  target="$1"  # e.g., agent-review/codex-1.md
  while [[ ! -f "$target" ]]; do sleep 2; done
  echo "Detected: $target"
  ```

## Optional Enhancements (if time permits)
- For recommended fixes, include pseudo‑diffs or snippet patches.
- Add a short “Assumptions & Constraints” section to the final report (e.g., single‑node deployment, no external cache).
- Add a short “Out‑of‑Scope” note to avoid scope creep.

## Tips for Better S‑Expression Understanding in Prompts
- Ask the reviewer to list the exact body forms of the target macro (e.g., `when`) before claiming inside/outside.
- Ask them to mark the specific closing parentheses lines for: the target form, the enclosing form, and the `defun`.
- Require a “structural outline” before any inside/outside claim.
- Encourage pretty‑printing when ambiguity remains.
