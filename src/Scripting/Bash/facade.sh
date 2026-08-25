#!/usr/bin/env bash
set -euo pipefail

authenticate() { printf 'auth(%s)' "$1"; }
reserve() { printf 'reserve(%s)' "$1"; }
charge() { printf 'charge(%s)' "$1"; }

checkout() {
  local user="$1" sku="$2" cents="$3"
  printf 'checkout=%s>%s>%s\n' "$(authenticate "$user")" "$(reserve "$sku")" "$(charge "$cents")"
}

checkout 'alice' 'SKU-42' 499
