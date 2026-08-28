#!/usr/bin/env bash
set -euo pipefail

declare -A peers=([a]=b [b]=a)
declare -a received=()
send_peer() {
  local from=$1 payload=$2 to
  to=${peers[$from]}
  received+=("$to<-$from:$payload")
}
send_peer a hello
send_peer b ack
[[ ${received[*]} == "b<-a:hello a<-b:ack" ]]
