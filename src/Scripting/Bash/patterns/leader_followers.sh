#!/usr/bin/env bash
set -euo pipefail

declare -a queue=(3 5)
declare -a handled=()
leader_follow() {
  local leader=$1
  local task=${queue[0]}
  queue=("${queue[@]:1}")
  handled+=("$leader:$task")
}
leader_follow worker-a
leader_follow worker-b
[[ ${handled[*]} == "worker-a:3 worker-b:5" ]]
[[ ${#queue[@]} -eq 0 ]]
