# Lección 12 — Límites, reconciliación y checkpoint 03

## Qué vas a conseguir

Cerrarás el bloque de tablas conectando capacidad explícita, agregados reconciliables y pruebas que demuestran que registros rechazados no contaminan resultados.

## Antes de empezar

Completa la [Lección 11](11-busqueda-ids-duplicados.md) y ejecuta `bash tests/smoke.sh` desde `app/`.

## El problema

Una tabla fija resuelve el problema actual, pero también crea un límite. Si fingimos que no existe, el programa puede fallar de forma difícil de explicar cuando el lote crezca. Los agregados por banda además pueden divergir del total global si se actualizan en momentos distintos.

## Concepto

NominaBatch declara capacidad para 100 IDs aceptados. Cuando el contador llega al límite, el siguiente registro se rechaza con `CAPACIDAD: máximo 100 IDs por lote`.

La invariancia importante es sencilla: cada registro aceptado pertenece exactamente a una banda. Por tanto, la suma de conteos de bandas debe coincidir con `WS-PROCESSED`, y los netos de bandas deben reconciliar con `WS-TOTAL-NET`.

## Demostración

[DEMO] Ejecuta el smoke e inspecciona `report.txt`. Debes observar tres procesados, siete rechazados y cuatro líneas de banda. El duplicado no modifica totales ni agregados.

## Código real

El diseño separa responsabilidades: `FIND-DUPLICATE-ID` consulta estado; `ACCUMULATE-BAND` clasifica; `WRITE-SUMMARY` reporta totales globales; `WRITE-BAND-SUMMARIES` recorre la tabla.

## Qué acaba de pasar

Ya puedes usar tablas pequeñas sin perder control del estado, explicar sus límites y comprobar invariantes.

## Errores comunes

- ocultar el límite;
- permitir que un rechazo actualice una banda;
- cambiar categorías sin actualizar pruebas;
- introducir sorting sin necesidad;
- confundir reconciliación con números que “se ven razonables”.

## Buenas prácticas

Define invariantes antes de optimizar. Para este lote pequeño una búsqueda lineal es clara. Si el volumen crece, mide antes de cambiar la estructura.

## Tu turno — Checkpoint 03

Resuelve [`../exercises/checkpoint-03.md`](../exercises/checkpoint-03.md) sin abrir la solución.

## Cómo comprobar

Ejecuta `bash tests/smoke.sh` y demuestra que la suma de empleados de las cuatro bandas coincide con `PROCESADOS`.

## Solución enlazada

Consulta [`../solutions/checkpoint-03.md`](../solutions/checkpoint-03.md) sólo después de intentar el ejercicio.

## Reto adicional

Diseña una prueba aislada para el registro 101 sin llenar el fixture canónico con cien filas manuales.

## Resumen

Las estructuras en memoria son útiles cuando su tamaño, contrato y límites son explícitos.

## Siguiente paso

El siguiente bloque llevará NominaBatch hacia organización profesional, tooling y diagnóstico antes de la evaluación final.

## Referencias

- [GnuCOBOL Manual](https://gnucobol.sourceforge.io/doc/gnucobol.html)
