#!/usr/bin/env bash
set -euo pipefail

legacy_read_fahrenheit() {
  printf '86\n'
}

adapter_read_celsius() {
  local fahrenheit
  fahrenheit="$(legacy_read_fahrenheit)"
  printf '%d\n' "$(((fahrenheit - 32) * 5 / 9))"
}

printf 'legacy=%sF\n' "$(legacy_read_fahrenheit)"
printf 'adapted=%sC\n' "$(adapter_read_celsius)"
