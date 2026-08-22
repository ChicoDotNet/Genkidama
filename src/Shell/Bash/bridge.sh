#!/usr/bin/env bash
set -euo pipefail

device_on() { printf '%s:on' "$1"; }
device_mute() { printf '%s:muted' "$1"; }

basic_remote() { device_on "$1"; }
mute_remote() { device_mute "$1"; }

printf 'basic-tv=%s\n' "$(basic_remote TV)"
printf 'basic-radio=%s\n' "$(basic_remote Radio)"
printf 'mute-tv=%s\n' "$(mute_remote TV)"
printf 'mute-radio=%s\n' "$(mute_remote Radio)"
