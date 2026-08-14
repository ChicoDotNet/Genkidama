# Lección 16 — Hardening operativo y checkpoint 04

## Qué vas a conseguir

Cerrarás el bloque profesional endureciendo la frontera HTTP y demostrando que diagnóstico, rutas y headers defensivos conservan contratos explícitos.

## Antes de empezar

Completa la [Lección 15](15-medir-antes-de-optimizar.md).

## El problema

Una API educativa que responde cualquier ruta con el dashboard y omite headers básicos enseña un contrato ambiguo. Además, un endpoint de diagnóstico encendido por defecto ampliaría superficie y exposición innecesarias.

## Concepto

Hardening aquí significa reducir ambigüedad y superficie con cambios pequeños:

- rutas desconocidas → 404;
- diagnóstico → 404 salvo opt-in explícito;
- `X-Content-Type-Options: nosniff`;
- `Referrer-Policy: no-referrer`;
- CSP acotada a la app actual.

Estos controles no sustituyen TLS, autenticación, autorización, rate limiting ni un reverse proxy de producción.

## Demostración

[DEMO] Compara `/`, `/health`, `/api/diagnostics` sin collector y `/ruta-inexistente`. Inspecciona status y headers con `curl -i`.

## Código real

`securityHeaders` envuelve el mux sin contaminar `monitor`, `history` o `insights`. `handleDashboard` acepta sólo `/` y evita que el patrón catch-all esconda rutas inexistentes.

La CSP permite por ahora el script inline del dashboard; es una limitación explícita. Un siguiente hardening real movería ese script a un recurso propio y eliminaría `'unsafe-inline'`.

## Qué acaba de pasar

La app tiene contratos HTTP más estrictos y reconoce qué riesgo cubre cada control y qué riesgo queda fuera.

## Errores comunes

- Llamar “segura” a una app por agregar tres headers.
- Exponer diagnóstico por defecto.
- Convertir 404 en dashboard 200 y ocultar errores de rutas.
- Añadir autenticación ficticia sin identidad real.

## Buenas prácticas

Prefiere controles verificables, mínimos y documentados. No hagas claims de seguridad mayores que la evidencia.

## Tu turno — Checkpoint 04

[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-04.md`](../exercises/checkpoint-04.md) sin abrir la solución.

## Cómo comprobar

```bash
gofmt -w .
go vet ./...
go test -race ./...
go build ./cmd/uptimelab
```

Verifica además 404 para una ruta desconocida, headers defensivos y diagnóstico opt-in.

## Solución enlazada

Después de tu intento, compara con [`../solutions/checkpoint-04.md`](../solutions/checkpoint-04.md).

## Reto adicional

Diseña el cambio mínimo para servir JavaScript desde `/app.js` y retirar `'unsafe-inline'` de CSP. No agregues framework.

## Resumen

UptimeLab ya cubre tooling, debugging, medición y hardening con pruebas reproducibles y límites honestos.

## Siguiente paso

La siguiente lección será la evaluación Junior autónoma sin receta.

## Referencias

- https://pkg.go.dev/net/http
- https://developer.mozilla.org/docs/Web/HTTP/Headers/Content-Security-Policy
- https://developer.mozilla.org/docs/Web/HTTP/Headers/X-Content-Type-Options
