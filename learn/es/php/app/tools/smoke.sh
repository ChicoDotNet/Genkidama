#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
port="${AGENDA_SMOKE_PORT:-8097}"
data_file="$(mktemp)"
log_file="$(mktemp)"
cookie_jar="$(mktemp)"
headers_file="$(mktemp)"
rm -f "$data_file"
cleanup() {
  if [[ -n "${server_pid:-}" ]]; then kill "$server_pid" 2>/dev/null || true; fi
  rm -f "$data_file" "$log_file" "$cookie_jar" "$headers_file"
}
trap cleanup EXIT

AGENDA_DATA_FILE="$data_file" AGENDA_TIMEZONE=UTC php -S "127.0.0.1:$port" -t public >"$log_file" 2>&1 &
server_pid=$!
for _ in {1..30}; do
  body="$(curl -fsS -c "$cookie_jar" -b "$cookie_jar" "http://127.0.0.1:$port/" 2>/dev/null || true)"
  if grep -q 'AgendaPHP' <<<"$body"; then break; fi
  sleep 0.2
done

home="$(curl -fsS -D "$headers_file" -c "$cookie_jar" -b "$cookie_jar" "http://127.0.0.1:$port/")"
grep -qi '^X-Content-Type-Options: nosniff' "$headers_file"
grep -qi '^Referrer-Policy: no-referrer' "$headers_file"
grep -qi '^Content-Security-Policy:' "$headers_file"
grep -q 'No hay citas que coincidan' <<<"$home"
csrf_token="$(sed -n 's/.*name="csrfToken" value="\([^"]*\)".*/\1/p' <<<"$home" | head -n1)"
test -n "$csrf_token"

forbidden_body="$(mktemp)"
forbidden_code="$(curl -sS -o "$forbidden_body" -w '%{http_code}' -c "$cookie_jar" -b "$cookie_jar" -X POST \
  --data-urlencode 'clientName=Intruso' \
  --data-urlencode 'serviceName=Sin token' \
  --data-urlencode 'startsAt=2026-08-20T08:00' \
  --data-urlencode 'durationMinutes=60' \
  "http://127.0.0.1:$port/")"
test "$forbidden_code" = "403"
grep -q 'solicitud no pudo verificarse' "$forbidden_body"
test ! -e "$data_file"
rm -f "$forbidden_body"

unsupported_body="$(mktemp)"
unsupported_code="$(curl -sS -o "$unsupported_body" -w '%{http_code}' -c "$cookie_jar" -b "$cookie_jar" -X POST \
  -H 'Content-Type: application/json' \
  --data '{"clientName":"Intruso"}' \
  "http://127.0.0.1:$port/")"
test "$unsupported_code" = "415"
test ! -e "$data_file"
rm -f "$unsupported_body"

curl -fsS -o /dev/null -c "$cookie_jar" -b "$cookie_jar" -X POST \
  --data-urlencode "csrfToken=$csrf_token" \
  --data-urlencode 'clientName=Cliente Demo' \
  --data-urlencode 'serviceName=Consulta' \
  --data-urlencode 'startsAt=2026-08-20T10:00' \
  --data-urlencode 'durationMinutes=60' \
  "http://127.0.0.1:$port/"

grep -q 'Cliente Demo' <<<"$(curl -fsS -c "$cookie_jar" -b "$cookie_jar" "http://127.0.0.1:$port/")"
appointment_id="$(php -r '$d=json_decode(file_get_contents($argv[1]), true, 512, JSON_THROW_ON_ERROR); echo $d[0]["id"];' "$data_file")"

edit_page="$(curl -fsS -c "$cookie_jar" -b "$cookie_jar" "http://127.0.0.1:$port/?edit=$appointment_id")"
grep -q 'Editar cita' <<<"$edit_page"
csrf_token="$(sed -n 's/.*name="csrfToken" value="\([^"]*\)".*/\1/p' <<<"$edit_page" | head -n1)"
curl -fsS -o /dev/null -c "$cookie_jar" -b "$cookie_jar" -X POST \
  --data-urlencode "csrfToken=$csrf_token" \
  --data-urlencode 'action=update' \
  --data-urlencode "id=$appointment_id" \
  --data-urlencode 'clientName=Cliente Demo' \
  --data-urlencode 'serviceName=Consulta actualizada' \
  --data-urlencode 'startsAt=2026-08-20T11:00' \
  --data-urlencode 'durationMinutes=90' \
  "http://127.0.0.1:$port/"

grep -q 'Consulta actualizada' <<<"$(curl -fsS -c "$cookie_jar" -b "$cookie_jar" "http://127.0.0.1:$port/")"
filtered="$(curl -fsS -c "$cookie_jar" -b "$cookie_jar" 'http://127.0.0.1:'"$port"'/?date=2026-08-20&service=actualizada')"
grep -q '1</strong> citas visibles' <<<"$filtered"
grep -q '90</strong> minutos reservados' <<<"$filtered"

empty_day="$(curl -fsS -c "$cookie_jar" -b "$cookie_jar" 'http://127.0.0.1:'"$port"'/?date=2026-08-21')"
grep -q '0</strong> citas visibles' <<<"$empty_day"

csv="$(curl -fsS -c "$cookie_jar" -b "$cookie_jar" 'http://127.0.0.1:'"$port"'/?date=2026-08-20&export=csv')"
grep -q 'Inicio,Cliente,Servicio,DuracionMinutos' <<<"$csv"
grep -q 'Consulta actualizada' <<<"$csv"

invalid_code="$(curl -sS -o /tmp/agenda-invalid-date.$$ -w '%{http_code}' -c "$cookie_jar" -b "$cookie_jar" 'http://127.0.0.1:'"$port"'/?date=2026-02-31')"
test "$invalid_code" = "422"
grep -q 'formato YYYY-MM-DD' /tmp/agenda-invalid-date.$$
rm -f /tmp/agenda-invalid-date.$$

home="$(curl -fsS -c "$cookie_jar" -b "$cookie_jar" "http://127.0.0.1:$port/")"
csrf_token="$(sed -n 's/.*name="csrfToken" value="\([^"]*\)".*/\1/p' <<<"$home" | head -n1)"
curl -fsS -o /dev/null -c "$cookie_jar" -b "$cookie_jar" -X POST \
  --data-urlencode "csrfToken=$csrf_token" \
  --data-urlencode 'action=cancel' \
  --data-urlencode "id=$appointment_id" \
  "http://127.0.0.1:$port/"
grep -q 'No hay citas que coincidan' <<<"$(curl -fsS -c "$cookie_jar" -b "$cookie_jar" "http://127.0.0.1:$port/")"

printf '{invalid-json' > "$data_file"
corrupt_body="$(mktemp)"
corrupt_code="$(curl -sS -o "$corrupt_body" -w '%{http_code}' -c "$cookie_jar" -b "$cookie_jar" "http://127.0.0.1:$port/")"
test "$corrupt_code" = "503"
grep -q 'No fue posible leer las citas guardadas' "$corrupt_body"
rm -f "$corrupt_body"
