#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
port="${AGENDA_SMOKE_PORT:-8097}"
data_file="$(mktemp)"
log_file="$(mktemp)"
rm -f "$data_file"
cleanup() {
  if [[ -n "${server_pid:-}" ]]; then kill "$server_pid" 2>/dev/null || true; fi
  rm -f "$data_file" "$log_file"
}
trap cleanup EXIT

AGENDA_DATA_FILE="$data_file" AGENDA_TIMEZONE=UTC php -S "127.0.0.1:$port" -t public >"$log_file" 2>&1 &
server_pid=$!
for _ in {1..30}; do
  body="$(curl -fsS "http://127.0.0.1:$port/" 2>/dev/null || true)"
  if grep -q 'AgendaPHP' <<<"$body"; then break; fi
  sleep 0.2
done

grep -q 'No hay citas que coincidan' <<<"$(curl -fsS "http://127.0.0.1:$port/")"
curl -fsS -o /dev/null -X POST \
  --data-urlencode 'clientName=Cliente Demo' \
  --data-urlencode 'serviceName=Consulta' \
  --data-urlencode 'startsAt=2026-08-20T10:00' \
  --data-urlencode 'durationMinutes=60' \
  "http://127.0.0.1:$port/"

grep -q 'Cliente Demo' <<<"$(curl -fsS "http://127.0.0.1:$port/")"
appointment_id="$(php -r '$d=json_decode(file_get_contents($argv[1]), true, 512, JSON_THROW_ON_ERROR); echo $d[0]["id"];' "$data_file")"

grep -q 'Editar cita' <<<"$(curl -fsS "http://127.0.0.1:$port/?edit=$appointment_id")"
curl -fsS -o /dev/null -X POST \
  --data-urlencode 'action=update' \
  --data-urlencode "id=$appointment_id" \
  --data-urlencode 'clientName=Cliente Demo' \
  --data-urlencode 'serviceName=Consulta actualizada' \
  --data-urlencode 'startsAt=2026-08-20T11:00' \
  --data-urlencode 'durationMinutes=90' \
  "http://127.0.0.1:$port/"

grep -q 'Consulta actualizada' <<<"$(curl -fsS "http://127.0.0.1:$port/")"
filtered="$(curl -fsS 'http://127.0.0.1:'"$port"'/?date=2026-08-20&service=actualizada')"
grep -q '1</strong> citas visibles' <<<"$filtered"
grep -q '90</strong> minutos reservados' <<<"$filtered"

empty_day="$(curl -fsS 'http://127.0.0.1:'"$port"'/?date=2026-08-21')"
grep -q '0</strong> citas visibles' <<<"$empty_day"

csv="$(curl -fsS 'http://127.0.0.1:'"$port"'/?date=2026-08-20&export=csv')"
grep -q 'Inicio,Cliente,Servicio,DuracionMinutos' <<<"$csv"
grep -q 'Consulta actualizada' <<<"$csv"

invalid_code="$(curl -sS -o /tmp/agenda-invalid-date.$$ -w '%{http_code}' 'http://127.0.0.1:'"$port"'/?date=2026-02-31')"
test "$invalid_code" = "422"
grep -q 'formato YYYY-MM-DD' /tmp/agenda-invalid-date.$$
rm -f /tmp/agenda-invalid-date.$$

curl -fsS -o /dev/null -X POST \
  --data-urlencode 'action=cancel' \
  --data-urlencode "id=$appointment_id" \
  "http://127.0.0.1:$port/"
grep -q 'No hay citas que coincidan' <<<"$(curl -fsS "http://127.0.0.1:$port/")"

printf '{invalid-json' > "$data_file"
corrupt_body="$(mktemp)"
corrupt_code="$(curl -sS -o "$corrupt_body" -w '%{http_code}' "http://127.0.0.1:$port/")"
test "$corrupt_code" = "503"
grep -q 'No fue posible leer las citas guardadas' "$corrupt_body"
rm -f "$corrupt_body"
