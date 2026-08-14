# Checkpoint 04 — Diagnóstico útil sin filtrar datos

Trabaja sobre UptimeLab después de la Lección 16.

## Encargo

1. Construye el servidor con diagnóstico agregado habilitado y un reloj determinista.
2. Fuerza una petición `/api/checks` que termine en HTTP 503.
3. Consulta `/api/diagnostics` y demuestra que contabiliza la petición, la falla y una duración conocida.
4. Usa un target con nombre y URL claramente privados dentro de la prueba y demuestra que **ninguno aparece** en el JSON diagnóstico.
5. Construye después el servidor normal, sin collector, y demuestra que `/api/diagnostics` devuelve 404.
6. Comprueba que una ruta inexistente devuelve 404 y que `/health` contiene los headers defensivos del curso.

No cambies `monitor.Result` ni persistas métricas en el historial para resolver el ejercicio.

## Criterios de aceptación

- `go test -race ./...` queda verde.
- El diagnóstico contiene sólo agregados.
- Diagnóstico está deshabilitado por defecto.
- No se debilita CSP ni se eliminan headers para pasar pruebas.
- Las reglas de monitoreo e historial permanecen separadas de HTTP.

Después de tu intento consulta [`../solutions/checkpoint-04.md`](../solutions/checkpoint-04.md).
