# Lección 08 — Centraliza el parser y protege regresiones

## Qué vas a conseguir

Reconocerás por qué contar, listar y resumir deben compartir un único camino de decodificación y cerrarás el segundo bloque con pruebas de regresión.

## El problema

Si cada comando interpreta los 17 bytes por su cuenta, tarde o temprano una ruta aceptará algo que otra rechaza. Eso es especialmente peligroso en formatos binarios.

## Concepto

La implementación usa helpers privados para validar header, leer el siguiente registro y decodificar little-endian. `telemetry_count_records`, `telemetry_read_records` y `telemetry_analyze_file` consumen ese mismo contrato.

Esta es separación de responsabilidad, no un Design Pattern que debamos bautizar. La razón es concreta: una sola interpretación del formato reduce divergencias.

## Pruebas que importan

La suite cubre round-trip, magic inválido, versión no soportada, registro truncado, capacidad insuficiente, conteo, resumen, archivo vacío y registros inválidos.

[EJECUTAR]

```bash
cmake --build app/build --parallel
ctest --test-dir app/build --output-on-failure
```

## Tu turno

Resuelve el [Checkpoint 02 — Analiza sin perder control de memoria](../exercises/checkpoint-02.md) antes de mirar la solución.

## Siguiente paso

Continúa con [Lección 09 — Filtra por sensor sin copiar el archivo](09-filtra-por-sensor-sin-copiar.md).
