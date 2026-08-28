#!/usr/bin/env bash
set -euo pipefail

here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
passed=0
for example in "$here"/patterns/*.sh; do
  "$BASH" "$example"
  passed=$((passed+1))
done
[[ $passed -eq 39 ]]
printf 'bash-pattern-sweep: %d/39 passed\n' "$passed"
