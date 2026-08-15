# Lección 01 — Tu primer check HTTP

## Qué vas a conseguir

Ejecutarás UptimeLab y comprenderás cómo Go representa un target, realiza una petición HTTP y devuelve un resultado explícito.

## Antes de empezar

Instala Go 1.26.x y comprueba `go version`. Desde `app/`, ejecuta `go test ./...`.

## El problema

Comprobar manualmente si varios servicios responden no escala. Empezaremos por una sola URL, pero desde código real que pueda evolucionar.

## Concepto

Go organiza código en paquetes. Un `struct` agrupa datos relacionados; un método agrega comportamiento. Las funciones que hacen I/O suelen devolver información suficiente para distinguir éxito de fallo.

## Demostración

[EN PANTALLA] Abre `app/monitor/monitor.go`. `Target` contiene nombre y URL. `Result` contiene status, latencia, instante y error. `Checker.Check` valida primero la URL y después construye una petición ligada a `context.Context`.

[EJECUTAR]

```bash
cd app
go test ./monitor -run TestCheckHealthyTarget -v
```

## Código real

`NewChecker(nil)` crea un `http.Client` con timeout. La aplicación no usa `http.Get` directamente porque queremos una frontera que pueda probarse e inyectarse.

`Healthy()` considera sanos los códigos 2xx/3xx sólo cuando no hubo error de transporte.

## Qué acaba de pasar

Un HTTP 500 y “no pude conectar” son situaciones distintas. El primero produjo respuesta HTTP; el segundo es un fallo operativo. Modelarlos explícitamente evita diagnósticos ambiguos.

## Errores comunes

- Omitir timeout y dejar una petición bloqueada demasiado tiempo.
- Aceptar una URL relativa o un esquema distinto de HTTP/HTTPS.
- Tratar cualquier respuesta como saludable.
- Olvidar cerrar `resp.Body`.

## Buenas prácticas

Valida en la frontera, devuelve errores legibles y conserva el núcleo pequeño. Usa la biblioteca estándar antes de agregar dependencias.

## Tu turno

[PAUSA PARA EJERCICIO] Añade un test para un servidor `httptest` que responda 503 y demuestra que `Healthy()` devuelve `false` sin convertir ese status en error de transporte.

## Cómo comprobar

```bash
go test ./...
```

## Solución enlazada

Compara tu prueba con el estilo de `monitor_test.go`; no necesitas una solución separada para este ejercicio corto.

## Reto adicional

Explica qué debería significar “saludable” para un redirect 302 y cuándo una organización podría decidir otra política.

## Resumen

Ya ejecutaste Go real, structs, métodos, errores y `net/http` dentro de la aplicación canónica.

## Siguiente paso

Continúa con [Tipos, errores y contratos](02-tipos-errores-y-contratos.md).

## Referencias

- https://go.dev/tour/basics/2
- https://pkg.go.dev/net/http
- https://pkg.go.dev/context
