#!/usr/bin/env bash
set -euo pipefail

declare -A CACHE=()
BACKEND_COUNT=0
FETCH_COUNT=0
REPLY=''

remote_get() {
  local id="$1"
  FETCH_COUNT=$((FETCH_COUNT + 1))
  REPLY="doc(${id})"
}

proxy_get() {
  local id="$1"
  if [[ -v "CACHE[$id]" ]]; then
    REPLY="${CACHE[$id]}"
    return
  fi

  if (( BACKEND_COUNT == 0 )); then
    BACKEND_COUNT=1
  fi

  remote_get "$id"
  CACHE[$id]="$REPLY"
}

proxy_get 42
first="$REPLY"
proxy_get 42
second="$REPLY"
printf 'backend=%d;fetches=%d;first=%s;second=%s\n' "$BACKEND_COUNT" "$FETCH_COUNT" "$first" "$second"
