# Lección 02 — Tipos, errores y contratos

## Qué vas a conseguir

Separarás datos, comportamiento e I/O usando structs, métodos, interfaces pequeñas y errores idiomáticos.

## Antes de empezar

Completa la [Lección 01](01-tu-primer-check-http.md) y deja `go test ./...` verde.

## El problema

Si el checker depende directamente de una implementación concreta de red y reloj, probar latencia, fallos y respuestas se vuelve frágil.

## Concepto

En Go una interfaz se satisface implícitamente. `Doer` declara únicamente `Do(*http.Request)`, el contrato mínimo que `Checker` necesita. `NewCheckerWithClock` permite además inyectar un reloj para pruebas deterministas.

Los errores forman parte del contrato: configuración inválida devuelve `error`; un fallo de un target particular queda en `Result.Error` para que un lote pueda continuar con los demás targets.

## Demostración

[DEMO] Recorre `Target`, `Result`, `Doer` y `Checker`. Observa que los identificadores exportados tienen comentarios GoDoc y que el paquete no conoce CLI ni dashboard.

[EJECUTAR]

```bash
go test ./monitor -v
go vet ./...
```

## Código real

`NewCheckerWithClock` rechaza cliente o clock `nil` en lugar de introducir un fallback silencioso. El constructor normal sí ofrece una política documentada: cliente estándar con timeout de cinco segundos.

La configuración `UPTIMELAB_TARGETS` usa `name=https://url`. Una entrada malformada aborta el proceso con un error accionable en vez de desaparecer silenciosamente.

## Qué acaba de pasar

La abstracción no se añadió por “usar interfaces”: aísla dos bordes reales —HTTP y tiempo— que necesitamos controlar en pruebas.

## Errores comunes

- Crear interfaces enormes “por arquitectura”.
- Usar `panic` ante entrada inválida de usuario.
- Devolver sólo `bool` cuando el caller necesita diagnóstico.
- Esconder configuraciones inválidas mediante defaults silenciosos.

## Buenas prácticas

Interfaces pequeñas en el consumidor, errores explícitos, cero globals mutables para reglas y comentarios GoDoc en API exportada.

## Tu turno

Escribe una prueba con un `Doer` falso que devuelva un error de transporte. Comprueba que `StatusCode` queda en cero y `Error` explica el fallo.

## Cómo comprobar

```bash
go test ./...
go vet ./...
```

## Solución enlazada

Usa como referencia la inyección que ya hace `web/server_test.go`; intenta primero tu propia implementación.

## Reto adicional

¿Cambiarías `Result.Error string` por un error estructurado si UptimeLab necesitara clasificar DNS, timeout y TLS? Argumenta el trade-off.

## Resumen

Ahora puedes explicar structs, métodos, interfaces implícitas, constructors y errores como contratos.

## Siguiente paso

Continúa con [Concurrencia acotada](03-concurrencia-acotada.md).

## Referencias

- https://go.dev/doc/effective_go#interfaces
- https://go.dev/blog/error-handling-and-go
- https://pkg.go.dev/net/http
