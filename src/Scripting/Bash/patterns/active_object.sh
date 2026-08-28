#!/usr/bin/env bash
set -euo pipefail

tmp=$(mktemp)
trap 'rm -f "$tmp"' EXIT
active_submit() {
  local value=$1
  ( printf '%s\n' $((value*2)) >"$tmp" ) &
  ACTIVE_PID=$!
}
active_submit 21
wait "$ACTIVE_PID"
[[ $(cat "$tmp") == 42 ]]
