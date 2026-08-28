#!/usr/bin/env bash
set -euo pipefail

visited=()
handled=''
result=''

handle() {
  local amount=$1
  local name limit
  while read -r name limit; do
    visited+=("$name")
    if [[ "$limit" == '*' || "$amount" -le "$limit" ]]; then
      handled="$name"
      result="refund($amount)"
      return 0
    fi
  done <<'CHAIN'
faq 50
billing 500
escalation *
CHAIN
  return 1
}

handle 250
visited_joined=$(IFS='>'; echo "${visited[*]}")
printf 'visited=%s;handled=%s;result=%s\n' "$visited_joined" "$handled" "$result"
