#!/usr/bin/env bash
set -euo pipefail

plain() {
  printf '%s' 'alert'
}

audit() {
  local inner="$1"
  printf 'audit(%s)' "$inner"
}

encrypt() {
  local inner="$1"
  printf 'enc(%s)' "$inner"
}

base="$(plain)"
audited="$(audit "$base")"
encrypted="$(encrypt "$base")"
stacked="$(audit "$(encrypt "$base")")"

printf 'base=%s\n' "$base"
printf 'audit=%s\n' "$audited"
printf 'encrypted=%s\n' "$encrypted"
printf 'stacked=%s\n' "$stacked"
