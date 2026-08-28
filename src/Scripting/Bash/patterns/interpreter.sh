#!/usr/bin/env bash
set -euo pipefail

interpret() {
  local expr=$1
  case "$expr" in
    "ADD "*)
      read -r _ a b <<<"$expr"
      echo $((a+b))
      ;;
    "MUL "*)
      read -r _ a b <<<"$expr"
      echo $((a*b))
      ;;
    *) return 2 ;;
  esac
}
[[ $(interpret "ADD 2 3") == 5 ]]
[[ $(interpret "MUL 4 5") == 20 ]]
! interpret "NOPE 1 2" >/dev/null
