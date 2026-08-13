# Lección 13 — Organizar un batch para poder cambiarlo

## Qué vas a conseguir

Vas a leer NominaBatch como un conjunto de responsabilidades y a reconocer cuándo un párrafo COBOL representa una frontera útil en vez de una simple división estética.

## Antes de empezar

Completa la [Lección 12](12-limites-reconciliacion-y-checkpoint.md) y ejecuta `bash tests/smoke.sh` desde `app/`.

## El problema

Cuando un batch crece, el riesgo no es sólo tener muchas líneas. El problema real aparece cuando leer, validar, calcular, acumular y escribir cambian juntos y ya no puedes predecir el impacto de una modificación.

## Concepto

En COBOL, un `paragraph` bien nombrado puede funcionar como una unidad de intención. No es una clase ni un módulo moderno, pero sí permite separar decisiones: `PROCESS-RECORD` orquesta una fila, `VALIDATE-AND-CALCULATE` protege reglas, `ACCUMULATE-BAND` actualiza agregados y `WRITE-*` concentra salida.

La separación útil conserva una dirección clara: **I/O → orquestación → reglas → resultados**.

## Demostración

[DEMO] Recorre `src/nomina.cob` desde `MAIN` sin leer todavía el cuerpo de cada párrafo. Explica qué hace el programa únicamente por sus nombres. Después entra a `VALIDATE-AND-CALCULATE` y señala qué estados deben permanecer intactos cuando el registro se rechaza.

## Código real

El copybook `copybooks/payroll-data.cpy` concentra el contrato estable del registro y los importes de trabajo. Las tablas de bandas e IDs permanecen en `WORKING-STORAGE` porque son estado del lote, no parte del contrato de una fila.

Esta distinción evita extraer datos a un copybook sólo para reducir líneas: una abstracción debe representar una frontera real.

## Qué acaba de pasar

Ya puedes razonar sobre el programa por responsabilidades antes de tocar sintaxis. Esa habilidad es más transferible que memorizar una lista de verbos COBOL.

## Errores comunes

- crear párrafos de una sola línea sin intención;
- compartir variables globales sin saber quién las modifica;
- mezclar rechazo y acumulación;
- mover todo a copybooks por apariencia;
- hacer un gran refactor sin una prueba que proteja comportamiento.

## Buenas prácticas

Antes de refactorizar, identifica invariantes: un rechazado no altera totales, un aceptado aparece una vez y cada aceptado pertenece exactamente a una banda. Mantén `bash tests/smoke.sh` verde mientras reorganizas.

## Tu turno

[PAUSA PARA EJERCICIO] Dibuja un mapa con cinco responsabilidades de NominaBatch y asigna cada párrafo actual a una. Si encuentras un párrafo con dos responsabilidades, propone una extracción pequeña sin implementarla todavía.

## Cómo comprobar

Tu mapa debe permitir explicar de dónde vienen los datos, dónde se validan, dónde se calculan importes, dónde se modifica estado del lote y dónde se escribe salida.

## Solución enlazada

No hay una única distribución correcta. Contrasta tu mapa con la estructura actual y conserva sólo separaciones que mejoren comprensión o pruebas.

## Reto adicional

Explica qué cambiarías si el cálculo de nómina tuviera que reutilizarse desde otro programa COBOL sin acceso a archivos.

## Resumen

Organizar no es fragmentar: es hacer explícitas responsabilidades e invariantes.

## Siguiente paso

Continúa con la [Lección 14 — tooling y gate profesional](14-tooling-y-gate-profesional.md).

## Referencias

- [GnuCOBOL Manual](https://gnucobol.sourceforge.io/doc/gnucobol.html)
