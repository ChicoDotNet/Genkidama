#!/usr/bin/env bash
set -euo pipefail

declare -A KIND=(
  [readme]=file
  [api]=file
  [guide]=file
  [docs]=folder
  [root]=folder
)
declare -A BYTES=(
  [readme]=2
  [api]=3
  [guide]=5
)
declare -A CHILDREN=(
  [docs]="api guide"
  [root]="readme docs"
)

size_node() {
  local node="$1"
  if [[ "${KIND[$node]}" == "file" ]]; then
    printf '%s\n' "${BYTES[$node]}"
    return
  fi

  local total=0 child child_size
  for child in ${CHILDREN[$node]}; do
    child_size="$(size_node "$child")"
    ((total += child_size))
  done
  printf '%d\n' "$total"
}

printf 'leaf=%s\n' "$(size_node readme)"
printf 'docs=%s\n' "$(size_node docs)"
printf 'root=%s\n' "$(size_node root)"
