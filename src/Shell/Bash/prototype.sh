#!/usr/bin/env bash
set -euo pipefail

clone_profile() {
  local source_name=$1
  local -n source_features=$2
  local target_name_var=$3
  local target_features_var=$4
  local -n target_name_ref=$target_name_var
  local -n target_features_ref=$target_features_var

  target_name_ref=$source_name
  target_features_ref=("${source_features[@]}")
}

describe() {
  local name=$1
  shift
  local features=("$@")
  local joined
  joined=$(IFS=,; printf '%s' "${features[*]}")
  printf '%s: %s' "$name" "$joined"
}

original_name="orders"
original_features=("metrics")
clone_name=""
clone_features=()

clone_profile "$original_name" original_features clone_name clone_features
clone_name="orders-canary"
clone_features+=("tracing")

printf 'original=%s\n' "$(describe "$original_name" "${original_features[@]}")"
printf 'clone=%s\n' "$(describe "$clone_name" "${clone_features[@]}")"
