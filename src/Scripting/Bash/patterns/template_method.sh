#!/usr/bin/env bash
set -euo pipefail

declare -a steps=()
validate() { steps+=(validate); }
persist() { steps+=(persist); }
after_persist_default() { steps+=(notify); }
after_persist_quiet() { :; }
save_template() {
  local hook=$1
  validate
  persist
  "$hook"
}
save_template after_persist_default
[[ ${steps[*]} == "validate persist notify" ]]
steps=()
save_template after_persist_quiet
[[ ${steps[*]} == "validate persist" ]]
