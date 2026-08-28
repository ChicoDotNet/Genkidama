#!/usr/bin/env bash
set -euo pipefail

declare -a observers=(audit cache)
declare -a notifications=()
audit() { notifications+=("audit:$1"); }
cache() { notifications+=("cache:$1"); }
notify() { local fn; for fn in "${observers[@]}"; do "$fn" "$1"; done; }
notify "order-9"
[[ ${notifications[*]} == "audit:order-9 cache:order-9" ]]
