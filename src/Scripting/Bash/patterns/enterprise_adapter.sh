#!/usr/bin/env bash
set -euo pipefail

legacy_lookup() { echo "LEGACY|$1|42"; }
adapt_customer() {
  local raw name id
  raw=$(legacy_lookup "$1")
  IFS='|' read -r _ name id <<<"$raw"
  echo "$id:$name"
}
[[ $(adapt_customer ALICE) == "42:ALICE" ]]
