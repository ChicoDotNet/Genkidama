#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
rm -f nomina report.txt
cobc -x -free -Wall -I copybooks -o nomina src/nomina.cob
./nomina

grep -F "E001|Demo Uno|10000.00|1000.00|9000.00" report.txt
grep -F "E002|Demo Dos|6317.50|505.40|5812.10" report.txt
grep -F "E009|Demo Nueve|8000.00|2000.00|6000.00" report.txt
grep -F "RECHAZADO|E003|HORAS fuera de rango 1..80" report.txt
grep -F "RECHAZADO|E004|FORMATO: se esperaban 5 campos" report.txt
grep -F "RECHAZADO|E005|HORAS no es numérico" report.txt
grep -F "RECHAZADO|E006|TARIFA debe ser mayor que cero" report.txt
grep -F "RECHAZADO|E007|DEDUCCIÓN debe estar entre 0 y 100" report.txt
grep -F "RECHAZADO||ID vacío" report.txt
grep -F "RECHAZADO|E001|ID duplicado en el lote" report.txt
grep -F "RESUMEN|PROCESADOS=3|RECHAZADOS=7|BRUTO=24317.50|DEDUCCIONES=3505.40|NETO=20812.10" report.txt
grep -F "BANDA|CERO|EMPLEADOS=0|NETO=0.00" report.txt
grep -F "BANDA|HASTA10|EMPLEADOS=2|NETO=14812.10" report.txt
grep -F "BANDA|HASTA20|EMPLEADOS=0|NETO=0.00" report.txt
grep -F "BANDA|MAS20|EMPLEADOS=1|NETO=6000.00" report.txt
printf 'NominaBatch smoke: OK\n'

bash tests/operational.sh
