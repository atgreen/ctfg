#!/usr/bin/env bash
set -euo pipefail
round=${1:-1}
base="agent-review"
while [[ ! -f "$base/claude-${round}.md" || ! -f "$base/gemini-${round}.md" ]]; do
  sleep 2
done
echo "Round ${round} critiques detected."

