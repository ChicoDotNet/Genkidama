# Lección 08 — Estado consistente y checkpoint 02

## Qué vas a conseguir

Cerrarás el segundo bloque demostrando una propiedad profesional: un check puede completar en red y aun así considerarse fallido si su historial no pudo persistirse; en ese caso el estado visible anterior permanece intacto.

## Antes de empezar

Completa la [Lección 07](07-scheduling-y-cancelacion.md).

## El problema

Hay dos resultados distintos que un programa ingenuo puede confundir:

1. “el endpoint respondió”; y
2. “el sistema registró durablemente esa observación”.

Si UptimeLab devuelve 200 y actualiza memoria aunque el archivo no se pudo guardar, el dashboard cuenta una historia distinta al disco. Eso complica reinicios y diagnóstico.

## Concepto

La secuencia de `web.Server.RunChecks` es deliberada:

```text
checker.CheckAll
      ↓
history.Log.Append
      ↓
Store.Save(candidate)
      ↓
commit en memoria
      ↓
respuesta exitosa
```

Una falla de persistencia corta la secuencia. El handler devuelve `503 Service Unavailable` y `Log.Entries()` conserva el snapshot anterior.

No es una transacción de base de datos distribuida; sí es un contrato local claro y testeable.

## Demostración

[EN PANTALLA] Abre `web/server_test.go` y localiza `TestChecksEndpointDoesNotPublishHistoryWhenPersistenceFails`.

La prueba usa un store falso que ya contiene una entrada `old` y falla al guardar. El checker produce `new`. El resultado esperado es:

- HTTP 503;
- el historial visible sigue teniendo sólo `old`.

[EJECUTAR]

```bash
cd app
go test -race ./...
```

## Código real

El mismo `RunChecks` es reutilizado por `/api/checks` y por el scheduler. La regla de consistencia no está duplicada en ambos callers.

La ruta `/api/history` sólo lee un snapshot defensivo; nunca obtiene el slice interno del log.

## Qué acaba de pasar

La frontera de persistencia dejó de ser “un detalle de implementación”: ahora forma parte de la semántica observable del sistema.

## Errores comunes

- Actualizar memoria antes de guardar y confiar en “arreglarlo luego”.
- Devolver 200 aunque la operación completa no terminó.
- Reintentar para siempre dentro del handler y bloquear requests.
- Hacer que la capa HTTP escriba directamente el archivo.
- Confundir un mutex con coordinación entre procesos.

## Buenas prácticas

Define qué significa éxito end-to-end, protege esa definición con una regresión y no ocultes fallas operativas detrás de valores por defecto.

## Tu turno — Checkpoint 02

[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-02.md`](../exercises/checkpoint-02.md) sin abrir la solución.

## Cómo comprobar

```bash
gofmt -w .
go vet ./...
go test -race ./...
go build ./cmd/uptimelab
```

Además ejecuta UptimeLab con `UPTIMELAB_INTERVAL=5s`, espera dos ciclos y confirma que `/api/history` crece y sobrevive al reinicio.

## Solución enlazada

Después de tu intento, compara con [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md).

## Reto adicional

Explica qué cambiarías si el historial tuviera millones de resultados. Habla de contrato de store, consultas y retención antes de elegir una base de datos concreta.

## Resumen

El segundo bloque añadió configuración tipada, historial durable, operación periódica y consistencia ante fallas de persistencia sin contaminar el checker.

## Siguiente paso

Continúa con [Lección 09 — Resúmenes derivados del historial](09-resumenes-derivados-del-historial.md).

## Referencias

- https://pkg.go.dev/net/http#StatusServiceUnavailable
- https://pkg.go.dev/sync#RWMutex
- https://go.dev/doc/effective_go#errors
