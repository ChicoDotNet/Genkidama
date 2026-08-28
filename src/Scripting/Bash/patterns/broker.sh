#!/usr/bin/env bash
set -euo pipefail

declare -A handlers=()
broker_register() { handlers["$1"]=$2; }
broker_call() { "${handlers[$1]}" "$2"; }
inventory() { echo "stock:$1"; }
broker_register inventory inventory
[[ $(broker_call inventory sku-9) == stock:sku-9 ]]
