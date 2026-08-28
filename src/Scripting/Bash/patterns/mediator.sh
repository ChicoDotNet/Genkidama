#!/usr/bin/env bash
set -euo pipefail

declare -a events=()
mediator_send() {
  local sender=$1 message=$2
  case "$sender" in
    checkout) events+=("inventory:$message" "billing:$message") ;;
    billing) events+=("receipt:$message") ;;
    *) return 2 ;;
  esac
}
mediator_send checkout order-7
mediator_send billing paid-7
[[ ${events[*]} == "inventory:order-7 billing:order-7 receipt:paid-7" ]]
