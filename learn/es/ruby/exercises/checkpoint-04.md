# Checkpoint 04 — Prepara ContactDesk para operación

Sin seguir una receta de código, entrega estas cuatro evidencias:

1. crea al menos 25 contactos y demuestra que la segunda página no carga más de 20 registros;
2. ejecuta `bin/rails contactdesk:diagnostics` y explica por qué su salida no contiene PII;
3. consulta `/healthz`, identifica el `request_id` y explica para qué sirve;
4. añade una prueba que demuestre que una consulta con `page=0` se normaliza a la primera página.

## Criterio de aceptación

- la suite completa sigue verde;
- no cambias `PAGE_SIZE` sólo para facilitar la prueba;
- la tarea y el health check permanecen de sólo lectura;
- no imprimes nombres, emails ni contenido de notas en diagnóstico;
- puedes explicar qué cambiaría si la tabla tuviera millones de filas.

Cuando termines, compara tu enfoque con la [solución de referencia](../solutions/checkpoint-04.md).
