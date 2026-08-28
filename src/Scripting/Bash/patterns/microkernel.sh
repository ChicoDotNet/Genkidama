#!/usr/bin/env bash
set -euo pipefail

declare -A plugins=()
register_plugin() { plugins["$1"]=$2; }
run_plugin() { "${plugins[$1]}" "$2"; }
uppercase() { printf '%s\n' "${1^^}"; }
register_plugin text uppercase
[[ $(run_plugin text hello) == HELLO ]]
