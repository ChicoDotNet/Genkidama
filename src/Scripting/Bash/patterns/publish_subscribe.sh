#!/usr/bin/env bash
set -euo pipefail

declare -A topics=()
declare -a seen=()
subscribe() { topics["$1"]="${topics[$1]-} $2"; }
emit() {
  local topic=$1 payload=$2 fn
  for fn in ${topics[$topic]-}; do "$fn" "$payload"; done
}
on_metric() { seen+=("metric:$1"); }
subscribe metrics on_metric
emit metrics 99
[[ ${seen[*]} == "metric:99" ]]
