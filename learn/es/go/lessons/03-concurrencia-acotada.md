# Lección 03 — Concurrencia acotada con goroutines

## Qué vas a conseguir

Ejecutarás varios checks HTTP concurrentemente sin perder el orden de entrada ni lanzar trabajo sin límite.

## Antes de empezar

Completa la [Lección 02](02-tipos-errores-y-contratos.md).

## El problema

Si cinco endpoints tardan un segundo cada uno, comprobarlos secuencialmente puede costar cerca de cinco segundos. Lanzar una goroutine sin control por target resuelve latencia, pero crea otro problema cuando el número crece.

## Concepto

`CheckAll` usa una goroutine por trabajo y un channel como semáforo. El channel limita cuántos checks pueden entrar simultáneamente a la región de I/O. `sync.WaitGroup` permite esperar a que terminen todos.

La función recibe `context.Context`: si el caller cancela antes de obtener un slot, el resultado registra la cancelación en vez de continuar trabajo innecesario.

## Demostración

[EN PANTALLA] Recorre `CheckAll`. Nota que cada goroutine escribe únicamente su posición en `results`; así la salida conserva el mismo orden de `targets`.

[EJECUTAR]

```bash
go test ./monitor -run TestCheckAllPreservesOrderAndLimitsConcurrency -v
```

La prueba usa contadores atómicos y un servidor local para demostrar que nunca hay más de dos requests activos.

## Código real

El parámetro `concurrency` es parte del contrato y rechaza valores menores a uno. UptimeLab no interpreta cero como “usa algo razonable” porque eso escondería una configuración defectuosa.

## Qué acaba de pasar

La concurrencia aparece porque las esperas de red son independientes. No convertimos cada función en goroutine ni usamos channels donde una llamada directa es más clara.

## Errores comunes

- Lanzar concurrencia ilimitada.
- Compartir un `append` entre goroutines sin sincronización.
- Confundir concurrencia con orden aleatorio de resultados.
- Ignorar cancelación.
- Añadir locks sin identificar qué memoria comparten las goroutines.

## Buenas prácticas

Define límites explícitos, conserva contratos deterministas donde sea útil y prueba la concurrencia con condiciones controladas, no con internet público.

## Tu turno

Añade un test con cuatro targets y concurrencia `1`. Demuestra que todos terminan y que el orden se conserva. Después cambia a `4` y explica qué cambia y qué no.

## Cómo comprobar

```bash
go test -race ./...
```

El detector de carreras agrega una señal importante cuando empiezas a compartir trabajo concurrente.

## Solución enlazada

No abras código nuevo: compara tu prueba con `TestCheckAllPreservesOrderAndLimitsConcurrency` y justifica las diferencias.

## Reto adicional

Diseña, sin implementarlo todavía, un worker pool persistente para 50,000 targets. ¿Qué contrato necesitarías para backpressure y shutdown?

## Resumen

Ya usas goroutines, channels, WaitGroup, context y race detector para resolver una necesidad real.

## Siguiente paso

Continúa con [API, dashboard y checkpoint 01](04-api-dashboard-y-checkpoint.md).

## Referencias

- https://go.dev/tour/concurrency/1
- https://pkg.go.dev/sync
- https://pkg.go.dev/context
- https://go.dev/doc/articles/race_detector
