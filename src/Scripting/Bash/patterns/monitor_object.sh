#!/usr/bin/env bash
set -euo pipefail

lock_dir=$(mktemp -d)
trap 'rm -rf "$lock_dir"' EXIT
counter=0
monitor_increment() {
  local guard="$lock_dir/guard"
  while ! mkdir "$guard" 2>/dev/null; do :; done
  counter=$((counter+1))
  rmdir "$guard"
}
monitor_increment
monitor_increment
[[ $counter -eq 2 ]]
