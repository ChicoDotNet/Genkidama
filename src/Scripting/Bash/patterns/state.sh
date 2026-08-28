#!/usr/bin/env bash
set -euo pipefail

state="new"
advance() {
  case "$state" in
    new) state="paid" ;;
    paid) state="shipped" ;;
    shipped) return 3 ;;
  esac
}
advance; [[ $state == paid ]]
advance; [[ $state == shipped ]]
! advance
