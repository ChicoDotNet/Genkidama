#!/usr/bin/env bash
set -euo pipefail

real_logger() { LOGGED=$1; }
null_logger() { :; }
service_run() {
  local logger=$1
  "$logger" "started"
  echo ok
}
[[ $(service_run null_logger) == ok ]]
LOGGED=""
service_run real_logger >/dev/null
[[ $LOGGED == started ]]
