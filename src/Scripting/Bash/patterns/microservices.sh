#!/usr/bin/env bash
set -euo pipefail

catalog_service() { [[ $1 == "sku-1" ]] && echo 25; }
tax_service() { echo $(( $1 / 5 )); }
checkout_service() {
  local price tax
  price=$(catalog_service "$1")
  tax=$(tax_service "$price")
  echo $((price+tax))
}
[[ $(checkout_service sku-1) == 30 ]]
