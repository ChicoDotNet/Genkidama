#!/usr/bin/env bash
set -euo pipefail

server_handle() {
  local request=$1
  [[ $request == "GET /health" ]] && echo "200 ok" || echo "404"
}
client_get() { server_handle "GET $1"; }
[[ $(client_get /health) == "200 ok" ]]
[[ $(client_get /missing) == 404 ]]
