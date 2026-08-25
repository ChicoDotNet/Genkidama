# Lección 07 — Distingue versión incompatible de datos truncados

## Qué vas a conseguir

Harás que TelemetryTape informe dos fallos distintos: una versión futura que este lector no entiende y un registro incompleto.

## El problema

`invalid_format` para todo obliga a adivinar. Una versión 2 bien formada no es lo mismo que tres bytes faltantes al final por una copia interrumpida.

## Concepto

El header se divide conceptualmente en:

- magic `GTL`: identifica la familia del archivo;
- versión `1`: identifica el contrato que sabemos decodificar.

La API devuelve `TELEMETRY_UNSUPPORTED_VERSION` cuando el magic es correcto pero la versión no está soportada. Si el último registro tiene menos de 17 bytes devuelve `TELEMETRY_TRUNCATED_RECORD`.

No se “repara” ninguno automáticamente. Un lector que modifica el archivo al detectar corrupción mezcla diagnóstico con remediación y puede destruir evidencia útil.

[DEMO]

Modifica una copia del cuarto byte del header de `1` a `2` y ejecuta `summary`. Después prueba una copia con tres bytes extra al final. Los diagnósticos deben ser diferentes.

## Tu turno

Añade una prueba que confirme que `out_count` queda en cero cuando el primer registro está truncado.

## Siguiente paso

[Lección 08 — Centraliza el parser y protege regresiones](08-centraliza-parser-y-protege-regresiones.md).
