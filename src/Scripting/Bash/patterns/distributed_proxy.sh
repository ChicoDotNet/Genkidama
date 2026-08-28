#!/usr/bin/env bash
set -euo pipefail

remote_inventory() { [[ $1 == sku-1 ]] && echo 8 || echo 0; }
inventory_proxy() {
  local sku=$1
  remote_inventory "$sku"
}
[[ $(inventory_proxy sku-1) == 8 ]]
