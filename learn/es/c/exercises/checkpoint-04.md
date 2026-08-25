# Checkpoint 04 — Diagnostica, recupera y entrega

Sin mirar la solución primero:

1. crea un `.gtl` con tres registros válidos;
2. agrega manualmente un sufijo truncado;
3. usa `diagnose` para identificar el prefijo válido;
4. recupera hacia **otra ruta** y demuestra que el original no cambió;
5. valida la copia recuperada con `diagnose` y `summary`;
6. ejecuta build + tests con warnings como errores;
7. instala TelemetryTape en un prefijo temporal con `cmake --install`.

## Evidencia esperada

- el diagnóstico del origen termina en `truncated_record`;
- la copia termina en `ok` y contiene tres registros;
- el origen continúa reportando truncado;
- CTest queda verde;
- el prefijo de instalación contiene CLI, header y biblioteca.

[Ver solución después de intentarlo](../solutions/checkpoint-04.md)
