# Lección 15 — Medir antes de optimizar

## Qué vas a conseguir

Añadirás diagnóstico agregado de peticiones HTTP sin registrar PII ni datos de targets, y usarás una medición reproducible para decidir si existe un problema antes de optimizar.

## Antes de empezar

Completa la [Lección 14](14-debugging-desde-evidencia.md).

## El problema

“Se siente lento” no es una métrica. Tampoco es aceptable resolverlo guardando cada URL, body o target en un log indefinido.

## Concepto

UptimeLab introduce `RequestMetrics`: sólo conserva número de peticiones, fallas 5xx y duración acumulada. La medición es **opt-in** y el reloj se inyecta en tests, evitando dependencias de tiempos reales.

## Demostración

[DEMO] Activa el collector al construir el servidor en una prueba, fuerza una falla 503 y consulta `/api/diagnostics`. Verás algo equivalente a:

```json
{"requests":1,"failures":1,"total_duration_ms":25}
```

No aparecen URL del target, nombre, body ni query string.

## Código real

`RequestMetrics` protege su estado con `sync.Mutex`; `Snapshot` devuelve una copia. El middleware mide alrededor de la frontera HTTP y sólo observa status/duración después de completar la respuesta.

## Qué acaba de pasar

Ahora puedes responder “cuántas peticiones fallan y cuánto tiempo agregado consumen” sin convertir diagnóstico en una base de datos sensible.

## Errores comunes

- Optimizar sin baseline.
- Tratar una única medición local como benchmark universal.
- Registrar targets completos por comodidad.
- Introducir Prometheus/OpenTelemetry antes de que exista una necesidad real.

## Buenas prácticas

Mide el mínimo dato que responde la pregunta. Si el volumen creciera, evalúa histogramas, percentiles y una plataforma de métricas, pero no simules esas garantías con tres contadores educativos.

## Tu turno

[PAUSA PARA EJERCICIO] Añade una aserción que demuestre que el diagnóstico no contiene el nombre ni la URL privada del target usado por la prueba.

## Cómo comprobar

```bash
go test ./web -run Diagnostics -v
go test -race ./...
```

## Solución enlazada

La suite del curso contiene la referencia: inspecciona el JSON devuelto y prueba ausencia de datos del target además de los agregados esperados.

## Reto adicional

Diseña, sin implementar, un histograma de latencia y explica por qué promedio y percentil p95 responden preguntas distintas.

## Resumen

Rendimiento defendible empieza por medir; observabilidad responsable empieza por minimizar datos.

## Siguiente paso

Continúa con [hardening y checkpoint 04](16-hardening-y-checkpoint-04.md).

## Referencias

- https://go.dev/doc/diagnostics
- https://pkg.go.dev/sync
- https://pkg.go.dev/time
