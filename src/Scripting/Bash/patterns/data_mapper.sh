#!/usr/bin/env bash
set -euo pipefail

declare -A rows=([7]="Ada|active")
domain_name=""
domain_active=0
mapper_load() {
  local id=$1 raw status
  raw=${rows[$id]}
  IFS='|' read -r domain_name status <<<"$raw"
  [[ $status == active ]] && domain_active=1 || domain_active=0
}
mapper_load 7
[[ $domain_name == Ada ]]
[[ $domain_active -eq 1 ]]
