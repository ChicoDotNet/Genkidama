#!/usr/bin/env bash
set -euo pipefail

tmp=$(mktemp)
trap 'rm -f "$tmp"' EXIT
async_stage() { ( echo $(( $1 * 2 )) >"$tmp" ) & ASYNC_PID=$!; }
sync_stage() { wait "$ASYNC_PID"; echo "handled:$(cat "$tmp")"; }
async_stage 6
[[ $(sync_stage) == handled:12 ]]
