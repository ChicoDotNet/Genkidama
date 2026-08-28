#!/usr/bin/env bash
set -euo pipefail

declare -A users=([1]=Ada [2]=Grace)
repo_get() { printf '%s\n' "${users[$1]-}"; }
repo_add() { users["$1"]=$2; }
[[ $(repo_get 1) == Ada ]]
repo_add 3 Linus
[[ $(repo_get 3) == Linus ]]
