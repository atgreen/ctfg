# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.4.0] - 2026-09-04

### Added

- Points-gated challenges: a challenge may declare `"minPoints"` in
  `challenges.json` and remains hidden until the player has earned that
  many points from solves. Hint purchases don't count against the
  threshold, so a revealed challenge never disappears. Combines with
  `requirements`; both conditions must be met before the challenge is
  revealed.
- Example points-gated bonus challenge, "The Commissioner's
  Commendation" (500 points, revealed at 1,500 earned points).
- This changelog. GitHub release notes are now generated from it.

### Fixed

- `/api/submit` now rejects flags for challenges that are not yet
  visible to the player (whether gated by `minPoints` or
  `requirements`), responding with the same `unknown_id` error as a
  nonexistent challenge, so hidden challenges can't be solved blind or
  probed for existence.
- The release workflow referenced a nonexistent step for its release
  name and body, producing releases with empty notes.
