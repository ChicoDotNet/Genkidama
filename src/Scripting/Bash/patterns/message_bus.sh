#!/usr/bin/env bash
set -euo pipefail

declare -A subscriptions=()
declare -a delivered=()
subscribe() { subscriptions["$1"]="${subscriptions[$1]-} $2"; }
publish() {
  local topic=$1 payload=$2 handler
  for handler in ${subscriptions[$topic]-}; do "$handler" "$payload"; done
}
audit_handler() { delivered+=("audit:$1"); }
email_handler() { delivered+=("email:$1"); }
subscribe order audit_handler
subscribe order email_handler
publish order 12
[[ ${delivered[*]} == "audit:12 email:12" ]]
