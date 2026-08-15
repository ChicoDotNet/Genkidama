# Lección 13 — Gate profesional de Go

## Qué vas a conseguir

Convertirás los comandos usados durante el curso en un gate reproducible: formato, análisis estático, pruebas con race detector y build.

## Antes de empezar

Completa la [Lección 12](12-diagnostico-reproducible-y-checkpoint.md).

## El problema

Que la aplicación “funcione en mi máquina” no demuestra que el código esté formateado, libre de errores detectables por `vet`, sin carreras cubiertas por las pruebas ni compilable desde un entorno limpio.

## Concepto

Go favorece tooling estándar y aburrido. Para UptimeLab el contrato local/CI es:

```bash
gofmt -w .
go vet ./...
go test -race ./...
go build ./cmd/uptimelab
```

`gofmt` elimina discusiones de estilo; `vet` detecta usos sospechosos; `-race` instrumenta accesos concurrentes durante los tests; `build` confirma el artefacto principal.

## Demostración

[DEMO] Introduce temporalmente un acceso concurrente inseguro en una copia descartable y observa cómo una regresión concurrente puede activar el race detector. Revierte el cambio antes de continuar.

## Código real

El workflow `Learn Go` ejecuta exactamente estos gates con Go 1.26.5 y además arranca el proceso real para comprobar `/health`.

## Qué acaba de pasar

El curso dejó de depender de una inspección manual aislada: humano y CI comparten el mismo contrato de calidad.

## Errores comunes

- Ejecutar sólo `go test` y olvidar concurrencia.
- Confundir `go vet` con una prueba funcional.
- Agregar linters pesados sin un problema que los justifique.
- Ignorar un rojo de CI porque “local pasa”.

## Buenas prácticas

Mantén el gate pequeño, rápido y reproducible. Un check debe proteger un riesgo concreto.

## Tu turno

[PAUSA PARA EJERCICIO] Rompe una expectativa de una prueba existente, observa el rojo, restaura el comportamiento y ejecuta el gate completo.

## Cómo comprobar

Ejecuta los cuatro comandos anteriores desde `app/`.

## Solución enlazada

No hay una solución única: la evidencia es que provocaste un rojo controlado y recuperaste el gate sin desactivar pruebas.

## Reto adicional

Explica qué añadirías si el proyecto publicara binarios para varias arquitecturas y qué no pertenece todavía a este curso.

## Resumen

Tooling profesional significa feedback reproducible, no una colección de herramientas.

## Siguiente paso

Continúa con [debugging desde evidencia](14-debugging-desde-evidencia.md).

## Referencias

- https://go.dev/cmd/gofmt/
- https://pkg.go.dev/cmd/vet
- https://go.dev/doc/articles/race_detector
- https://pkg.go.dev/cmd/go#hdr-Test_packages
