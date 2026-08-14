# Lección 06 — Historial persistente con JSON y errores explícitos

## Qué vas a conseguir

Añadirás historial persistente a UptimeLab mediante una frontera `history.Store`, un `FileStore` JSON y un `Log` acotado que mantiene memoria y disco sincronizados.

## Antes de empezar

Completa la [Lección 05](05-configuracion-operativa.md).

## El problema

Un monitor que olvida todo al reiniciar no puede responder preguntas básicas: ¿qué pasó hace una hora?, ¿el endpoint viene fallando repetidamente?, ¿el último check sobrevivió a un restart?

Persistir directamente desde el handler HTTP resolvería el síntoma, pero acoplaría transporte, filesystem y política de retención.

## Concepto

`history.Store` define sólo dos operaciones:

```go
type Store interface {
    Load() ([]monitor.Result, error)
    Save([]monitor.Result) error
}
```

`history.Log` conoce la política de retención y sincronización; `FileStore` conoce JSON y archivos. La capa web sólo pide “registrar estos resultados”.

El archivo inexistente representa una primera ejecución y produce historial vacío. Un archivo que existe pero contiene JSON corrupto produce error. Silenciarlo como si estuviera vacío escondería pérdida o corrupción de datos.

## Demostración

[EJECUTAR]

```bash
cd app
go test ./history
UPTIMELAB_HISTORY_FILE=data/demo-history.json go run ./cmd/uptimelab
```

Ejecuta `/api/checks`, detén el proceso y vuelve a levantarlo. Luego consulta:

```bash
curl http://127.0.0.1:8080/api/history
```

[EN PANTALLA] Observa `history/history_test.go`: hay regresiones para round-trip, primera ejecución, JSON corrupto y límite de entradas.

## Código real

`Log.Append` construye un snapshot candidato y pide al store que lo guarde **antes** de reemplazar `entries`. Esta secuencia evita un estado engañoso: si el disco falla, la memoria no anuncia un resultado que no pudo persistirse.

El historial conserva como máximo las últimas 200 observaciones en la aplicación real. El límite evita crecimiento indefinido de un demo local; no pretende sustituir una base temporal de producción.

## Qué acaba de pasar

UptimeLab ya tiene estado durable sin convertir `monitor.Checker` en una clase “que hace de todo”.

## Errores comunes

- Tratar JSON corrupto como archivo inexistente.
- Escribir memoria primero y disco después sin rollback.
- Devolver el slice interno y permitir mutaciones externas.
- Guardar historial ilimitado por defecto.
- Registrar secretos o payloads; aquí persistimos únicamente metadata del check.

## Buenas prácticas

Copia slices en fronteras públicas, propaga errores con contexto usando `%w` y mantén I/O fuera del paquete que decide salud/concurrencia.

## Tu turno

[PAUSA PARA EJERCICIO] Cambia temporalmente el límite a 2 en una prueba y demuestra que al insertar tres resultados sobreviven sólo los dos más recientes.

## Cómo comprobar

```bash
gofmt -w .
go vet ./...
go test -race ./...
```

## Solución enlazada

La prueba `TestAppendKeepsOnlyNewestEntries` muestra una referencia después de que intentes el ejercicio.

## Reto adicional

Explica qué necesitarías para que dos procesos distintos escriban el mismo historial sin perder actualizaciones. No lo resuelvas con un mutex: un mutex de proceso no coordina procesos distintos.

## Resumen

Persistencia es una frontera explícita, los errores de corrupción son visibles y el estado en memoria sólo avanza después de una escritura exitosa.

## Siguiente paso

Continúa con la [Lección 07 — Scheduling y cancelación](07-scheduling-y-cancelacion.md): automatizaremos checks sin goroutines ocultas dentro del dominio.

## Referencias

- https://pkg.go.dev/encoding/json
- https://pkg.go.dev/os
- https://pkg.go.dev/sync#RWMutex
