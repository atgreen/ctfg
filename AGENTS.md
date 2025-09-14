# Repository Guidelines

## Project Structure & Module Organization
- Root: `ctfg.asd` (ASDF system), `Makefile`, `index.html`, `challenges.json`, `credentials.csv`, `game-clusters.yaml`.
- Source (Common Lisp): `src/` — files loaded via `ctfg.asd` in declared order. Add new modules here and update `:components` in `ctfg.asd`.
- Frontend assets: `css/ctfg.css`, `js/app.js`, images in `images/`. Asset tarball `runtime-files.tgz` is generated; do not hand‑edit.
- Tests: `tests/` — Playwright specs are generated from `tests/make_tests.sh` using `tests/credentials.csv`.
- Artifacts: `ctfg` (executable), `events.db*` (SQLite), `playwright-report/`.

## Build, Test, and Development Commands
- Build: `make` — compiles the SBCL program via ASDF and produces `./ctfg`.
- Run (dev): `./ctfg -d` — developer mode (no asset cache, live challenge reloads).
- Test suite: `make check` — builds, generates test specs, then runs `npx playwright test`.
- Clean: `make clean` — removes binary, DB files, generated tests/artifacts.
- Manual tests: `cd tests && ./make_tests.sh 10 && npx playwright test`.

## Coding Style & Naming Conventions
- Common Lisp: 2‑space indentation; package is `ctfg`. Use kebab‑case for symbols (e.g., `save-solve`), docstrings on public defs, align keyword params, avoid global state unless needed. One feature per file; file names `kebab-case.lisp`.
- JavaScript (`js/app.js`): 2 spaces, semicolons, strict mode. Prefer small pure helpers; reuse existing `SecurityUtils` and `UIUtils`. Avoid direct HTML injection; sanitize input.
- Assets: keep Tailwind utility classes in HTML; edit `css/ctfg.css` directly when needed.

## Testing Guidelines
- Framework: Playwright (`playwright.config.ts` starts `./ctfg -d` on port 8080).
- Spec naming: `tests/ctfg-<username>.spec.ts` (generated). Keep custom specs under `tests/` with `.spec.ts` suffix.
- Data: ensure `tests/credentials.csv` exists; tests derive users from it.
- Reports: HTML report in `playwright-report/`.

## Commit & Pull Request Guidelines
- Commits: short, imperative subject (e.g., "Fix hint purchase error"). Group related changes; include brief body if behavior changes.
- PRs: include purpose, key changes, how to test (`make check` steps), and screenshots for UI updates. Link issues. Avoid committing secrets or `events.db*`.

## Security & Configuration Tips
- Use `.env` for secrets (e.g., `CTFG_API_TOKEN`); never commit it.
- DB path is configurable via `-b/--dbdir`. WebSocket URL via `-w/--websocket-url`.
- Reset local state by deleting `events.db*` (Playwright does this automatically).
