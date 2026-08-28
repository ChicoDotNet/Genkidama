#!/usr/bin/env bash
set -euo pipefail

visit_invoice() { echo "invoice:$1"; }
visit_order() { echo "order:$1"; }
accept() {
  local type=$1 id=$2
  "visit_${type}" "$id"
}
[[ $(accept invoice 4) == invoice:4 ]]
[[ $(accept order 8) == order:8 ]]
