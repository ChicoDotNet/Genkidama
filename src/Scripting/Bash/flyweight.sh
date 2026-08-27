#!/usr/bin/env bash
set -euo pipefail

declare -A styles=()
style_count=0

intern_style() {
  local font="$1" size="$2" color="$3" key="${1}|${2}|${3}"
  if [[ -z "${styles[$key]+x}" ]]; then
    styles[$key]="$key"
    ((style_count+=1))
  fi
}

intern_style Inter 12 red
red1="${styles[Inter|12|red]}"
intern_style Inter 12 red
red2="${styles[Inter|12|red]}"
intern_style Inter 12 blue

shared=false
[[ "$red1" == "$red2" ]] && shared=true
printf 'styles=%d;shared=%s;text=ABC\n' "$style_count" "$shared"
