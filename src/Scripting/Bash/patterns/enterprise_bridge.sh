#!/usr/bin/env bash
set -euo pipefail

send_email() { echo "email:$1"; }
send_sms() { echo "sms:$1"; }
notify_bridge() {
  local channel=$1 payload=$2
  "send_${channel}" "$payload"
}
[[ $(notify_bridge email ready) == email:ready ]]
[[ $(notify_bridge sms ready) == sms:ready ]]
