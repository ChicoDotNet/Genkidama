#!/usr/bin/env bash
set -euo pipefail

model=0
declare -a rendered=()
abstraction_increment() { model=$((model+1)); }
presentation_render() { rendered+=("value=$model"); }
control_click() { abstraction_increment; presentation_render; }
control_click
[[ ${rendered[*]} == "value=1" ]]
