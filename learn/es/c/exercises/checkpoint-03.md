# Checkpoint 03 — Consulta sin romper el contrato

Trabaja sobre TelemetryTape después de la lección 12.

## Encargo

Necesitas entregar a un analista únicamente las muestras del sensor 42 capturadas entre 10 000 ms inclusive y 20 000 ms exclusivo.

1. Genera un `.gtl` con al menos cinco registros; incluye muestras en 9 999, 10 000, 19 999 y 20 000 ms y mezcla sensores 42 y 7.
2. Usa `query` para demostrar cuáles registros pertenecen exactamente al intervalo.
3. Exporta la misma consulta a CSV.
4. Añade o ajusta una prueba para demostrar que 20 000 queda fuera.
5. Corrompe **una copia** del archivo agregando menos de 17 bytes y demuestra que un nuevo `log` es rechazado. No destruyas tu fixture sano.

## Evidencia observable

- La consulta incluye 10 000 y 19 999 cuando corresponden al sensor 42.
- Excluye 9 999, 20 000 y cualquier otro sensor.
- CSV y query representan el mismo conjunto y orden.
- El archivo truncado produce un error explícito en lugar de aceptar un append.
- CTest sigue verde.

## Restricción

No cargues todo el archivo para implementar la consulta. La capacidad ya existe por streaming; úsala y explica por qué preserva ownership.

[Consulta la solución sólo después de intentarlo](../solutions/checkpoint-03.md).
