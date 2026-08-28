#!/usr/bin/env bash
set -euo pipefail

balance=100
withdraw() { balance=$((balance-$1)); }
declare -a queue=("withdraw:20" "withdraw:5")
for command in "${queue[@]}"; do
  IFS=: read -r action amount <<<"$command"
  "$action" "$amount"
done
[[ $balance -eq 75 ]]
