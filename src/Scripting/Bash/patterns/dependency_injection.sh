#!/usr/bin/env bash
set -euo pipefail

real_clock() { echo 1200; }
fake_clock() { echo 900; }
service_now() {
  local clock=$1
  "$clock"
}
[[ $(service_now real_clock) == 1200 ]]
[[ $(service_now fake_clock) == 900 ]]
