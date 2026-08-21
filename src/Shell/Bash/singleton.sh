#!/usr/bin/env bash
set -euo pipefail

REGISTRY_COUNT=0

registry_instance() {
  printf '%s\n' registry
}

first="$(registry_instance)"
second="$(registry_instance)"
((REGISTRY_COUNT += 1))

if [[ "$first" == "$second" ]]; then
  printf 'same=true\n'
else
  printf 'same=false\n'
fi
printf 'count=%d\n' "$REGISTRY_COUNT"
