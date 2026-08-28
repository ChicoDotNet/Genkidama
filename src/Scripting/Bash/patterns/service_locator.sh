#!/usr/bin/env bash
set -euo pipefail

declare -A services=()
register_service() { services["$1"]=$2; }
locate() { LOCATED=${services[$1]-}; [[ -n $LOCATED ]]; }
clock_service() { echo 1200; }
register_service clock clock_service
locate clock
[[ $("$LOCATED") == 1200 ]]
! locate missing
