# Lección 11 — Contratos HTTP para diagnóstico

## Qué vas a conseguir

Harás que los datos derivados sean consumibles desde la API con contratos pequeños, status codes claros y validación en la frontera, sin trasladar detalles HTTP al análisis.

## Antes de empezar

Completa la [Lección 10](10-tendencias-por-ventanas.md).

## El problema

Una función correcta puede quedar inutilizable si el endpoint acepta parámetros ambiguos, responde 200 ante entradas inválidas o devuelve estructuras cuyo orden cambia entre ejecuciones.

## Concepto

UptimeLab trata la capa HTTP como adaptador:

- `/api/summary` no recibe parámetros y deriva un snapshot actual;
- `/api/trends?window=N` convierte texto a entero y limita `N` a `1..100`;
- errores del caller producen 400;
- JSON usa tipos exportados del paquete `insights`;
- el análisis no sabe que existe `http.Request`.

Este reparto permite probar `insights` sin servidor y probar el contrato HTTP con `httptest`.

## Demostración

[EN PANTALLA] Revisa `web/server_test.go` y localiza las pruebas de summary, trends y ventana inválida.

[EJECUTAR]

```bash
cd app
go test -race ./web -run 'Summary|Trends'
```

## Código real

`Server.historyEntries()` centraliza el snapshot vacío o persistido. Ambos endpoints leen ese snapshot y nunca adquieren directamente el mutex de `history.Log`.

La UI del dashboard usa `/api/summary` después de ejecutar checks. Sigue siendo deliberadamente pequeña: el curso estudia Go y contratos operativos, no un framework frontend.

## Qué acaba de pasar

El sistema tiene ahora dos capas de prueba complementarias: funciones puras para reglas derivadas y handlers HTTP para parsing/status/JSON.

## Errores comunes

- Aceptar `window=-1` y dejar que produzca slices inválidos.
- Devolver 500 por un parámetro del cliente.
- Acoplar `insights.Trends` a query strings.
- Añadir un framework sólo para parsear una URL y escribir JSON.

## Buenas prácticas

Valida temprano en la frontera, conserva errores semánticos en la capa que los conoce y mantén tests offline con `httptest`.

## Tu turno

[PAUSA PARA EJERCICIO] Añade una prueba para `window=101`. Debe producir exactamente 400 y no ejecutar checks ni modificar historial.

## Cómo comprobar

```bash
gofmt -w .
go vet ./...
go test -race ./...
```

## Solución enlazada

El comportamiento canónico está en `app/web/server.go`; el ejercicio integrador tiene solución separada en la siguiente lección.

## Reto adicional

Propón una representación de error JSON estable (`code`, `message`) y explica cuándo valdría la pena sustituir `http.Error`.

## Resumen

Los endpoints de diagnóstico son adaptadores del historial y `insights`; no nuevas fuentes de verdad.

## Siguiente paso

Continúa con [Lección 12 — Diagnóstico reproducible y checkpoint 03](12-diagnostico-reproducible-y-checkpoint.md).

## Referencias

- https://pkg.go.dev/net/http
- https://pkg.go.dev/net/http/httptest
- https://go.dev/doc/effective_go#errors
