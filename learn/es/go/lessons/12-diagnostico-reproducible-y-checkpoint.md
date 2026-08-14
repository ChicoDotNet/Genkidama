# Lección 12 — Diagnóstico reproducible y checkpoint 03

## Qué vas a conseguir

Cerrarás el tercer bloque usando historial, resúmenes y tendencias para diagnosticar un deterioro de servicio de forma reproducible y sin confundir datos derivados con estado durable.

## Antes de empezar

Completa la [Lección 11](11-contratos-http-para-diagnostico.md).

## El problema

Cuando un servicio “se siente inestable”, mirar sólo el último status code invita a conclusiones rápidas. Necesitas evidencia suficiente para separar un incidente puntual de una tendencia.

## Concepto

UptimeLab ofrece tres niveles de lectura:

1. `/api/history`: evidencia cruda y durable;
2. `/api/summary`: estado agregado de toda la muestra retenida;
3. `/api/trends`: comparación reciente vs anterior.

Ninguno sustituye al otro. Si la retención es 200, las métricas representan esa muestra local, no un SLA histórico universal.

## Demostración

[DEMO] Usa un target de laboratorio que alterne respuestas sanas y fallidas. Ejecuta varios ciclos y compara:

```bash
curl http://127.0.0.1:8080/api/history
curl http://127.0.0.1:8080/api/summary
curl 'http://127.0.0.1:8080/api/trends?window=2'
```

Explica qué afirmación puedes sostener con cada respuesta y cuál no.

## Código real

`insights.Summarize` y `insights.Trends` operan sobre copias del historial. No toman locks internos, no escriben archivos y no dependen del scheduler. Eso permite ejecutar consultas mientras otros goroutines hacen checks sin compartir slices mutables.

## Qué acaba de pasar

Construiste diagnóstico por capas: observaciones, agregados y comparación. También mantuviste determinismo y límites explícitos.

## Errores comunes

- Presentar disponibilidad de una muestra corta como SLA.
- Persistir summary/trends y crear dos verdades.
- Ignorar que la retención descarta resultados antiguos.
- Optimizar una consulta pequeña antes de medir.
- Convertir un delta negativo en causa raíz; sólo es una señal.

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

Demuestra además el endpoint con una ventana válida y dos inválidas.

## Solución enlazada

Después de tu intento, compara con [`../solutions/checkpoint-03.md`](../solutions/checkpoint-03.md).

## Reto adicional

Diseña un endpoint de SLO mensual y enumera qué cambios de almacenamiento/retención exigiría antes de poder responder honestamente.

## Resumen

El tercer bloque convirtió historial en diagnóstico útil mediante funciones puras, contratos HTTP validados y pruebas reproducibles.

## Siguiente paso

El siguiente bloque llevará UptimeLab a tooling profesional, debugging, medición de rendimiento y hardening antes de la evaluación final.

## Referencias

- https://go.dev/doc/diagnostics
- https://pkg.go.dev/net/http/httptest
- https://go.dev/doc/articles/race_detector
