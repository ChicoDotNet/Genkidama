# Lección 10 — Consulta intervalos temporales sin ambigüedad

## Qué vas a conseguir

Combinarás sensor y tiempo usando un contrato de intervalo explícito: **inicio incluido, fin excluido** (`[start, end)`).

## El problema

Si una consulta termina exactamente en 3000 ms y la siguiente empieza en 3000 ms, un intervalo cerrado en ambos lados puede contar el mismo registro dos veces.

## Concepto

TelemetryTape acepta límites opcionales. Cuando existen ambos exige `start < end` y aplica:

```text
start_timestamp_ms <= timestamp_ms < end_timestamp_ms
```

Esto permite encadenar ventanas consecutivas sin huecos ni duplicados.

[DEMO]

```bash
./app/build/telemetry_cli query sample.gtl '*' 1000 3000
./app/build/telemetry_cli query sample.gtl 7 1000 3000
```

## Error explícito

`3000 3000` y `4000 3000` son filtros inválidos. La CLI devuelve error de entrada y la API devuelve `TELEMETRY_INVALID_ARGUMENT`.

## Tu turno

Crea muestras exactamente en 1000, 2000 y 3000. Comprueba que `[1000,3000)` devuelve las dos primeras y excluye la tercera.

## Reflexión

La precisión temporal no es sólo sintaxis. Es parte del contrato de negocio/técnico del formato y debe probarse en sus fronteras.

## Siguiente paso

Continúa con [Lección 11 — Exporta CSV de forma reproducible](11-exporta-csv-reproducible.md).
