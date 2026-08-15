# Lección 10 — Tendencias por ventanas

## Qué vas a conseguir

Compararás una ventana reciente de disponibilidad contra la ventana inmediatamente anterior para detectar deterioro o mejora sin introducir una librería estadística.

## Antes de empezar

Completa la [Lección 09](09-resumenes-derivados-del-historial.md).

## El problema

Una disponibilidad global de 98% puede parecer saludable aunque los últimos checks estén fallando. El promedio histórico responde “cómo nos ha ido”; una tendencia responde “¿estamos empeorando ahora?”.

## Concepto

`insights.Trends(results, window)` agrupa por target y compara:

```text
ventana anterior → ventana reciente → delta
```

Con cuatro observaciones `healthy, healthy, unhealthy, unhealthy` y `window=2`, la ventana anterior tiene 100%, la reciente 0% y el delta es -100 puntos porcentuales.

El contrato usa **puntos porcentuales**, no “porcentaje de cambio”. Son conceptos diferentes.

## Demostración

[EJECUTAR]

```bash
cd app
go test -race ./insights -run Trend
```

La prueba también protege que `window=0` sea un error explícito.

## Código real

La API publica:

```text
GET /api/trends?window=5
```

Si se omite `window`, UptimeLab usa 5. Un valor fuera de `1..100` devuelve `400 Bad Request`. La frontera HTTP valida strings; `insights` recibe un entero ya tipado pero conserva su propia precondición mínima.

## Qué acaba de pasar

Separaste validación de transporte de una regla de análisis reutilizable. También construiste una señal operacional útil sin convertir una muestra pequeña en una promesa estadística.

## Errores comunes

- Confundir puntos porcentuales con porcentaje relativo.
- Comparar ventanas que se solapan sin documentarlo.
- Inventar una tendencia cuando no existe muestra anterior.
- Hacer que `insights` lea query strings.

## Buenas prácticas

Una métrica necesita semántica clara. Para un curso local usamos ventanas por número de muestras; un producto real podría usar ventanas temporales, SLOs y percentiles según su dominio.

## Tu turno

[PAUSA PARA EJERCICIO] Escribe una prueba con cinco muestras y `window=3`. Identifica exactamente cuáles pertenecen a la ventana reciente y cuáles a la anterior.

## Cómo comprobar

```bash
go test -race ./...
curl 'http://127.0.0.1:8080/api/trends?window=2'
```

Prueba también `window=0` y confirma HTTP 400.

## Solución enlazada

La función de referencia está en `app/insights/insights.go`.

## Reto adicional

Diseña, sin implementar, una tendencia basada en tiempo (`últimos 15 min` vs `15 min anteriores`). ¿Qué harías con checks irregulares?

## Resumen

Las tendencias complementan el promedio histórico y hacen visible el cambio reciente sin alterar la fuente durable.

## Siguiente paso

Continúa con [Lección 11 — Contratos HTTP para diagnóstico](11-contratos-http-para-diagnostico.md).

## Referencias

- https://pkg.go.dev/net/url#Values
- https://pkg.go.dev/strconv#Atoi
- https://go.dev/doc/effective_go#errors
