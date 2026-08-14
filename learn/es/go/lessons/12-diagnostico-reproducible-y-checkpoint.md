# Lección 12 — Diagnóstico reproducible y checkpoint 03

## Qué vas a conseguir

Cerrarás el tercer bloque usando historial, resúmenes y tendencias para diagnosticar un deterioro sin confundir datos derivados con estado durable.

## Antes de empezar

Completa la [Lección 11](11-contratos-http-para-diagnostico.md).

## El problema

Mirar sólo el último status code invita a conclusiones rápidas. Necesitas evidencia suficiente para separar un incidente puntual de una tendencia.

## Concepto

UptimeLab ofrece tres niveles: `/api/history` como evidencia cruda, `/api/summary` como agregado de la muestra y `/api/trends` como comparación reciente/anterior. Ninguno sustituye al otro ni constituye por sí mismo un SLA.

## Demostración

[DEMO] Ejecuta varios ciclos y compara:

```bash
curl http://127.0.0.1:8080/api/history
curl http://127.0.0.1:8080/api/summary
curl 'http://127.0.0.1:8080/api/trends?window=2'
```

## Código real

`insights.Summarize` y `insights.Trends` operan sobre copias del historial. No escriben archivos ni dependen del scheduler.

## Qué acaba de pasar

Construiste diagnóstico por capas: observaciones, agregados y comparación.

## Errores comunes

- Presentar una muestra corta como SLA.
- Persistir summary/trends y crear dos verdades.
- Convertir un delta negativo en causa raíz.
- Optimizar antes de medir.

## Buenas prácticas

Describe siempre ventana, muestra y definición de salud. Usa métricas para reducir hipótesis, no para inventar causalidad.

## Tu turno — Checkpoint 03

[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-03.md`](../exercises/checkpoint-03.md) sin abrir la solución.

## Cómo comprobar

```bash
cd app
gofmt -w .
go vet ./...
go test -race ./...
go build ./cmd/uptimelab
```

## Solución enlazada

Después de tu intento, compara con [`../solutions/checkpoint-03.md`](../solutions/checkpoint-03.md).

## Reto adicional

Diseña un endpoint de SLO mensual y enumera qué cambios de almacenamiento/retención exigiría.

## Resumen

El tercer bloque convirtió historial en diagnóstico útil mediante funciones puras y contratos validados.

## Siguiente paso

Continúa con el [gate profesional de Go](13-gate-profesional-de-go.md).

## Referencias

- https://go.dev/doc/diagnostics
- https://pkg.go.dev/net/http/httptest
- https://go.dev/doc/articles/race_detector
