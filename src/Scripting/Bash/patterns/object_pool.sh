#!/usr/bin/env bash
set -euo pipefail

declare -a pool=(conn-a conn-b)
acquire() {
  ((${#pool[@]})) || return 3
  ACQUIRED=${pool[0]}
  pool=("${pool[@]:1}")
}
release() { pool+=("$1"); }
acquire
[[ $ACQUIRED == conn-a ]]
release "$ACQUIRED"
[[ ${pool[*]} == "conn-b conn-a" ]]
