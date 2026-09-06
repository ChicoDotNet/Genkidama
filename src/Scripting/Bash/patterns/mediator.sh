#!/usr/bin/env bash
set -euo pipefail

declare -a events=()

inventory_receive() {
  local sender=$1 message=$2
  events+=("inventory<-${sender}:${message}")
}

payment_receive() {
  local sender=$1 message=$2
  events+=("payment<-${sender}:${message}")
}

mediator_send() {
  local sender=$1 recipient=$2 message=$3
  case "$recipient" in
    inventory) inventory_receive "$sender" "$message" ;;
    payment) payment_receive "$sender" "$message" ;;
    *) return 2 ;;
  esac
}

payment_colleague() {
  mediator_send payment inventory "$1"
}

inventory_colleague() {
  mediator_send inventory payment "$1"
}

payment_colleague paid
inventory_colleague reserved
[[ ${events[*]} == "inventory<-payment:paid payment<-inventory:reserved" ]]

if mediator_send payment unknown ignored; then
  echo "unknown colleague unexpectedly accepted" >&2
  exit 1
fi
