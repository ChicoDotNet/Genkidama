# Solución — Checkpoint 03

Una secuencia posible es crear el archivo, agregar registros de frontera y consultar:

```bash
./app/build/telemetry_cli init checkpoint03.gtl
./app/build/telemetry_cli log checkpoint03.gtl 9999 42 10 0
./app/build/telemetry_cli log checkpoint03.gtl 10000 42 20 0
./app/build/telemetry_cli log checkpoint03.gtl 15000 7 30 0
./app/build/telemetry_cli log checkpoint03.gtl 19999 42 40 1
./app/build/telemetry_cli log checkpoint03.gtl 20000 42 50 1
./app/build/telemetry_cli query checkpoint03.gtl 42 10000 20000
./app/build/telemetry_cli export checkpoint03.gtl checkpoint03.csv 42 10000 20000
```

La consulta debe devolver exactamente los timestamps `10000` y `19999`. `20000` queda fuera porque el contrato es `[start,end)`.

Para la prueba automatizada no dependas de inspección visual: construye un `telemetry_filter`, usa un visitor que copie sólo las coincidencias a un fixture pequeño y afirma timestamps/count exactos.

Para truncado, trabaja con una copia desechable. El punto técnico es que `telemetry_append_record` valida todo el stream mediante `telemetry_count_records` antes de abrir en `ab`; el error `truncated_record` se propaga y el archivo no se prolonga.

## Trade-off

Validar todo antes de cada append cuesta O(n) por escritura. Es correcto para este curso porque hace visible el contrato de integridad. Una evolución de alto volumen podría usar bloques con checksum, footer transaccional o un índice, pero requeriría un formato nuevo y pruebas de recuperación; no conviene introducirlo de forma invisible.
