# Lección 17 — Evaluación final sin receta

## Qué vas a conseguir

Demostrarás que puedes leer, modificar, probar y explicar UptimeLab sin seguir una receta paso a paso. Esta lección integra concurrencia, cancelación, persistencia, HTTP, diagnóstico, seguridad básica y tooling de Go.

## Antes de empezar

Completa la [Lección 16](16-hardening-y-checkpoint-04.md). Desde `app/` ejecuta:

```bash
gofmt -w .
go vet ./...
go test -race ./...
go build ./cmd/uptimelab
```

## El problema

Un equipo usa UptimeLab para vigilar endpoints internos. Necesita una evolución pequeña, pero exige conservar orden determinista, límites de concurrencia, historial durable, diagnóstico sin datos sensibles y los contratos HTTP existentes.

No recibirás una lista de archivos ni funciones que debas modificar.

## Concepto

Una evaluación profesional no mide memoria de sintaxis. Mide si puedes **leer → formular una hipótesis → probar → implementar → diagnosticar → verificar → explicar**.

## Demostración

[DEMO] Antes de cambiar nada, recorre `monitor`, `history`, `insights`, `scheduler`, `web` y `cmd/uptimelab`. Explica qué responsabilidad pertenece a cada paquete y qué dependencia sería sospechosa en sentido contrario.

## Código real

Abre [`../exercises/evaluacion-final.md`](../exercises/evaluacion-final.md) y resuelve el encargo sobre la misma aplicación canónica. Puedes consultar las lecciones, mensajes del compilador, `go doc`, `pkg.go.dev` y documentación oficial.

No abras la solución antes de completar un intento serio.

## Qué acaba de pasar

Ya no estás siguiendo instrucciones de implementación: estás manteniendo una base existente con contratos que debes descubrir y conservar.

## Errores comunes

- Agregar estado derivado como una segunda fuente durable de verdad.
- Romper el orden de resultados para “hacer más concurrente” el código.
- Lanzar goroutines sin límite, cancelación o ownership claro.
- Mutar memoria antes de confirmar persistencia.
- Registrar URLs o nombres de targets dentro del diagnóstico.
- Corregir un bug sin regresión.
- Optimizar sin medir.

## Buenas prácticas

Mantén reglas puras donde sea posible, I/O en fronteras, errores envueltos con contexto, APIs exportadas documentadas y pruebas offline/deterministas. Conserva `go test -race ./...` como parte del contrato profesional.

## Tu turno

[PAUSA PARA EJERCICIO] Completa las historias A–F de la evaluación. Después prepara una explicación de cinco minutos sobre arquitectura, concurrencia, consistencia, diagnóstico, hardening y un tradeoff que hayas aceptado.

## Cómo comprobar

Como mínimo:

```bash
gofmt -w .
go vet ./...
go test -race ./...
go build ./cmd/uptimelab
```

Además arranca UptimeLab y prueba manualmente el flujo modificado, una entrada inválida, una falla de persistencia y una ruta desconocida. Usa la [`rúbrica final`](../exercises/rubrica-final.md) para autoevaluarte.

## Solución enlazada

Sólo después de tu intento, compara con [`../solutions/evaluacion-final.md`](../solutions/evaluacion-final.md). La referencia describe una dirección válida; no exige código idéntico.

## Reto adicional

Explica qué cambiaría si varias instancias de UptimeLab escribieran el mismo historial. No implementes una base distribuida: identifica contratos, coordinación, consistencia y riesgos.

## Cómo hablar de este proyecto en una entrevista

Cuenta primero el problema: checks HTTP concurrentes con evidencia durable. Después explica por qué limitaste concurrencia, cómo preservas orden, por qué `context.Context` vive en operaciones cancelables, cómo evitas estado fantasma, cómo derivas insights sin duplicar verdad, qué cubre `-race`, por qué el diagnóstico no retiene URLs y qué límites tiene el hardening actual.

Preguntas probables:

- ¿Por qué no lanzar una goroutine por target sin límite?
- ¿Qué diferencia hay entre un HTTP 500 y un error de transporte?
- ¿Cómo garantizas que una falla al guardar no deje memoria inconsistente?
- ¿Qué detecta y qué no detecta el race detector?
- ¿Por qué summary y trends no se persisten?
- ¿Qué datos deliberadamente no guarda RequestMetrics?
- ¿Qué cambiarías para múltiples procesos o millones de observaciones?

## Resumen

Completar el curso significa poder modificar una aplicación Go real, demostrar el comportamiento con pruebas y explicar sus decisiones. Es evidencia de preparación inicial; no garantiza contratación.

## Siguiente paso

Repite las áreas débiles de la rúbrica, conserva UptimeLab como evidencia y construye una variante propia sin copiar la solución.

## Referencias

- https://go.dev/doc/
- https://pkg.go.dev/context
- https://pkg.go.dev/net/http
- https://go.dev/doc/articles/race_detector
- https://go.dev/doc/diagnostics
