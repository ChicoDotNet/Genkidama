#!/usr/bin/env bash
set -euo pipefail

declare -A styles=()
style_count=0

get_style() {
  local font="$1" size="$2" color="$3" key="${1}|${2}|${3}"
  if [[ -z "${styles[$key]+x}" ]]; then
    styles[$key]="$key"
    ((style_count+=1))
  fi
  printf '%s' "${styles[$key]}"
}

red1="$(get_style Inter 12 red)"
# command substitution runs in a subshell, so call through the shared table directly here
get_style Inter 12 red >/dev/null
red2="${styles[Inter|12|red]}"
get_style Inter 12 blue >/dev/null

shared=false
[[ "$red1" == "$red2" ]] && shared=true
printf 'styles=%d;shared=%s;text=ABC\n' "$style_count" "$shared"
