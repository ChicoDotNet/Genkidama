# Lección 09 — Resúmenes derivados del historial

## Qué vas a conseguir

Convertirás el historial crudo de UptimeLab en información útil por objetivo: muestras, disponibilidad, latencia media, último estado y racha de fallas, sin duplicar ni persistir datos derivados.

## Antes de empezar

Completa la [Lección 08](08-estado-consistente-y-checkpoint.md) y confirma que `/api/history` contiene varios checks.

## El problema

Una lista de respuestas HTTP es evidencia, pero no responde rápido preguntas operativas como: “¿qué servicio está peor?”, “¿cuál lleva varias fallas?” o “¿qué disponibilidad observamos en esta muestra?”.

## Concepto

El paquete `insights` recibe `[]monitor.Result` y devuelve vistas derivadas. No conoce archivos ni HTTP. La historia durable sigue siendo la fuente de verdad; un resumen puede reconstruirse siempre.

`Summary` incluye:

- `Samples` y `Healthy`;
- `AvailabilityPercent`;
- `AverageLatency`;
- `ConsecutiveFailures`;
- último instante y último estado.

Agrupamos por nombre + URL y ordenamos las claves para que la salida sea determinista.

## Demostración

[EN PANTALLA] Abre `app/insights/insights.go` y sigue `Summarize`.

[EJECUTAR]

```bash
cd app
go test -race ./insights/...
```

Observa que la prueba mezcla dos targets y espera que `api` aparezca antes que `web`, independientemente del orden del `map`.

## Código real

La API expone ahora:

```text
GET /api/summary
```

El handler obtiene un snapshot defensivo del historial y llama a `insights.Summarize`. No guarda otro archivo `summary.json` porque sería estado derivable susceptible de quedar desincronizado.

## Qué acaba de pasar

Usaste slices y maps para construir una vista útil sin acoplar análisis, persistencia y transporte.

## Errores comunes

- Confiar en el orden de iteración de un `map`.
- Persistir métricas que pueden recalcularse del historial pequeño.
- Calcular disponibilidad usando sólo status code y olvidar errores de transporte.
- Exponer el slice interno mutable de `history.Log`.

## Buenas prácticas

Mantén funciones derivadas puras cuando sea posible. Ordena explícitamente si el contrato de salida necesita determinismo. Documenta qué significa “healthy”; aquí sigue siendo 2xx/3xx sin error de transporte.

## Tu turno

[PAUSA PARA EJERCICIO] Añade una prueba con tres fallas consecutivas y una recuperación final. ¿Qué valor debe tener `ConsecutiveFailures`?

## Cómo comprobar

```bash
gofmt -w .
go vet ./...
go test -race ./...
```

Después abre `/api/summary` y comprueba que el resumen cambia después de ejecutar `/api/checks`.

## Solución enlazada

La implementación canónica vive en `app/insights/insights.go`; el checkpoint integrado aparece en la Lección 12.

## Reto adicional

Explica por qué un promedio puede ocultar picos de latencia y qué estadístico añadirías antes de intentar optimizar.

## Resumen

El historial sigue siendo evidencia durable; `insights` lo convierte en información reconstruible y determinista.

## Siguiente paso

Continúa con [Lección 10 — Tendencias por ventanas](10-tendencias-por-ventanas.md).

## Referencias

- https://go.dev/blog/maps
- https://pkg.go.dev/sort
- https://go.dev/doc/effective_go#slices
