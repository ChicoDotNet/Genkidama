#!/usr/bin/env bash
set -euo pipefail

model_name="Ada"
vm_name() { printf '%s\n' "$model_name"; }
vm_rename() { model_name=$1; }
view_text() { echo "Hello $(vm_name)"; }
[[ $(view_text) == "Hello Ada" ]]
vm_rename Grace
[[ $(view_text) == "Hello Grace" ]]
