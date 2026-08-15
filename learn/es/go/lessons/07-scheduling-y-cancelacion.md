# Lección 07 — Scheduling periódico y cancelación limpia

## Qué vas a conseguir

Harás que UptimeLab ejecute checks automáticamente mediante un `scheduler.Runner` pequeño, cancelable y separado del monitor HTTP.

## Antes de empezar

Completa la [Lección 06](06-historial-persistente.md) y confirma que puedes consultar `/api/history` después de reiniciar.

## El problema

Hasta ahora alguien debe abrir el dashboard o llamar `/api/checks`. Un monitor real necesita trabajo periódico, pero “poner un `for` infinito con `Sleep`” mezcla ciclo de vida, errores y cancelación.

## Concepto

El scheduler recibe dos cosas:

- un `time.Duration` positivo;
- una función `func(context.Context) error`.

No conoce targets, HTTP ni archivos. Ejecuta la operación inmediatamente y luego una vez por tick. Si el contexto se cancela, termina; si la operación falla, devuelve el error en lugar de esconderlo.

## Demostración

[EJECUTAR]

```bash
cd app
UPTIMELAB_INTERVAL=10s go run ./cmd/uptimelab
```

Espera algunos ciclos y consulta:

```bash
curl http://127.0.0.1:8080/api/history
```

Detén con `Ctrl+C`. El mismo contexto de proceso cancela el runner y dispara el shutdown HTTP.

[EN PANTALLA] `scheduler/scheduler_test.go` no espera una hora: el primer callback cancela el contexto inmediatamente y la prueba verifica que sólo ocurrió una ejecución.

## Código real

`main` compone:

```text
signal context
   ├── http.Server
   └── scheduler.Runner → web.Server.RunChecks → history.Log
```

El scheduler no crea goroutines por sí mismo. El caller decide lanzarlo con `go`, haciendo visible el ownership del ciclo de vida.

## Qué acaba de pasar

La operación continua quedó fuera del dominio y sigue siendo cancelable/testeable. La misma operación `RunChecks` sirve tanto al endpoint manual como al scheduler.

## Errores comunes

- Crear goroutines internas que el caller no puede detener.
- Usar `time.Sleep` sin escuchar cancelación.
- Ignorar errores del callback y continuar produciendo fallos silenciosos.
- Crear un ticker con intervalo cero o negativo.
- Usar `context.Background()` dentro del scheduler y perder la señal del proceso.

## Buenas prácticas

Quien crea una goroutine debe saber cuándo termina. Pasa `context.Context` por operaciones bloqueantes y conserva explícita la política ante errores.

## Tu turno

[PAUSA PARA EJERCICIO] Modifica una prueba para que el callback devuelva `errors.New("boom")` y comprueba que `Run` termina con ese error sin esperar al siguiente tick.

## Cómo comprobar

```bash
gofmt -w .
go vet ./...
go test -race ./...
```

## Solución enlazada

Después de intentarlo, compara con el flujo actual de `Runner.Run`: el error del callback se devuelve inmediatamente.

## Reto adicional

Diseña una política de “continuar después de error” con backoff. ¿Debería vivir dentro de este Runner o en una capa superior? Justifica sin implementarla.

## Resumen

UptimeLab ya puede operar periódicamente con lifecycle explícito, cancelación y errores visibles.

## Siguiente paso

Continúa con la [Lección 08 — Estado consistente y checkpoint 02](08-estado-consistente-y-checkpoint.md), donde integraremos persistencia y scheduling bajo un contrato de éxito end-to-end.

## Referencias

- https://pkg.go.dev/time#Ticker
- https://pkg.go.dev/context
- https://pkg.go.dev/os/signal#NotifyContext
