# Lección 04 — API, dashboard y checkpoint 01

## Qué vas a conseguir

Expondrás el monitor mediante HTTP/JSON y un dashboard local, y cerrarás el primer checkpoint modificando una política real de timeout.

## Antes de empezar

Completa la [Lección 03](03-concurrencia-acotada.md) y ejecuta `go test -race ./...`.

## El problema

Un checker útil necesita una superficie desde la que otros programas o una persona puedan consultar resultados. No queremos mezclar HTML, JSON y reglas de monitoreo dentro del mismo paquete.

## Concepto

`web.Server` recibe una interfaz `BatchChecker`. La capa web conoce rutas HTTP y serialización; `monitor.Checker` conoce checks y concurrencia. El ejecutable conecta ambas piezas.

`net/http` en Go 1.26 soporta patrones con método, por ejemplo `GET /api/checks`. Para este vertical no necesitamos framework externo.

## Demostración

[EJECUTAR]

```bash
cd app
go run ./cmd/uptimelab
```

Abre `http://127.0.0.1:8080`. Después consulta:

```bash
curl http://127.0.0.1:8080/health
curl http://127.0.0.1:8080/api/checks
```

[EN PANTALLA] `web/server_test.go` prueba el handler con `httptest` y un checker falso; ninguna prueba depende de internet.

## Código real

La ruta `/health` responde 204 si el proceso puede atender requests. `/api/checks` ejecuta el lote configurado. `/` entrega un HTML mínimo que usa Fetch para mostrar JSON.

El dashboard es deliberadamente pequeño: el curso es de Go, no de CSS/JavaScript. Más adelante añadiremos historial y operación continua antes de invertir en presentación.

## Qué acaba de pasar

La misma app ya tiene dominio operativo, concurrencia, API, UI mínima y proceso ejecutable. Las fronteras permiten probar cada responsabilidad por separado.

## Errores comunes

- Hacer requests reales a internet desde unit tests.
- Meter reglas de salud dentro del handler HTTP.
- Convertir `/health` en una llamada a todos los targets y volverlo lento/inestable.
- Añadir un framework sólo para tener routing básico.

## Buenas prácticas

Prueba handlers con `httptest`, mantén interfaces mínimas, usa timeouts y documenta qué garantiza cada endpoint.

## Tu turno — Checkpoint 01

[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-01.md`](../exercises/checkpoint-01.md) sin abrir la solución.

## Cómo comprobar

```bash
gofmt -w .
go vet ./...
go test -race ./...
go build ./cmd/uptimelab
```

## Solución enlazada

Después de tu intento, compara con [`../solutions/checkpoint-01.md`](../solutions/checkpoint-01.md).

## Reto adicional

Explica cómo separarías “el proceso está vivo” de “todos los servicios monitoreados están saludables” en un despliegue real.

## Resumen

UptimeLab ya es una aplicación pequeña pero real: ejecuta checks concurrentes y los expone por API/dashboard con pruebas locales.

## Siguiente paso

Continúa con la [Lección 05 — Configuración operativa](05-configuracion-operativa.md), donde el proceso traduce variables externas a contratos Go tipados sin contaminar el dominio.

## Referencias

- https://pkg.go.dev/net/http
- https://pkg.go.dev/net/http/httptest
- https://go.dev/doc/tutorial/web-service-gin (como contraste de ecosistema; este curso aún usa biblioteca estándar)
