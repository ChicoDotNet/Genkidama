#!/usr/bin/env bash
set -euo pipefail

declare -a pending=()
declare -a committed=()
register_new() { pending+=("insert:$1"); }
register_dirty() { pending+=("update:$1"); }
commit() { committed+=("${pending[@]}"); pending=(); }
register_new 7
register_dirty 8
commit
[[ ${committed[*]} == "insert:7 update:8" ]]
[[ ${#pending[@]} -eq 0 ]]
