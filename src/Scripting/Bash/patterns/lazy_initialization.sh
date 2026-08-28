#!/usr/bin/env bash
set -euo pipefail

resource=""
build_count=0
get_resource() {
  if [[ -z $resource ]]; then
    resource="connection"
    build_count=$((build_count+1))
  fi
  LAZY_VALUE=$resource
}
get_resource
get_resource
[[ $LAZY_VALUE == connection ]]
[[ $build_count -eq 1 ]]
