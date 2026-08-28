#!/usr/bin/env bash
set -euo pipefail

inventory_reserve() { echo "reserved:$1"; }
billing_charge() { echo "charged:$1"; }
place_order() {
  local id=$1
  printf '%s;%s\n' "$(inventory_reserve "$id")" "$(billing_charge "$id")"
}
[[ $(place_order 7) == "reserved:7;charged:7" ]]
