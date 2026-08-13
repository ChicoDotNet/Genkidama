# Lección 16 — Operación confiable y checkpoint 04

## Qué vas a conseguir

Cerrarás el bloque profesional comprobando que NominaBatch conserva resultados claros tanto en ejecución normal como cuando un archivo requerido no está disponible.

## Antes de empezar

Completa la [Lección 15](15-diagnostico-y-rendimiento.md) y ejecuta `bash tools/verify.sh`.

## El problema

Una aplicación batch necesita contratos observables. No basta con calcular importes: también debe indicar de forma inequívoca si pudo leer la entrada, crear el reporte y completar el lote.

## Concepto

NominaBatch usa `FILE STATUS`, códigos de retorno y validación previa a los acumuladores. `tests/operational.sh` complementa el smoke funcional con escenarios aislados de archivos y comprueba que el programa informa correctamente su resultado.

## Demostración

[EJECUTAR]

```bash
bash tools/verify.sh
```

Después compara `tests/smoke.sh` y `tests/operational.sh`: uno protege resultados de negocio y el otro protege contratos de ejecución.

## Código real

Los párrafos `FAIL-INPUT-OPEN`, `FAIL-REPORT-OPEN`, `FAIL-INPUT-READ` y `ENSURE-REPORT-WRITE` mantienen las decisiones operativas fuera de la lógica de cálculo. Los registros rechazados siguen sin modificar totales ni bandas.

## Qué acaba de pasar

El gate del curso ya cubre tanto el camino funcional como resultados operativos importantes.

## Errores comunes

- asumir que producir un binario equivale a probar el programa;
- mezclar manejo de archivos con cálculo monetario;
- modificar el fixture canónico para cada escenario de prueba;
- ocultar límites o resultados de ejecución.

## Buenas prácticas

Mantén escenarios reproducibles y temporales. Cada prueba debe proteger un contrato que puedas explicar a otra persona.

## Tu turno — Checkpoint 04

Resuelve [`../exercises/checkpoint-04.md`](../exercises/checkpoint-04.md) sin abrir la solución.

## Cómo comprobar

```bash
bash tools/verify.sh
```

Tu solución del checkpoint debe añadir una regresión específica para la invariancia que implementes.

## Solución enlazada

Consulta [`../solutions/checkpoint-04.md`](../solutions/checkpoint-04.md) sólo después de completar un intento.

## Reto adicional

Describe cómo automatizarías la ejecución diaria de NominaBatch sin cambiar su lógica de negocio.

## Resumen

Una aplicación batch profesional necesita resultados de negocio correctos y contratos operativos comprobables.

## Siguiente paso

Continúa con la [Lección 17 — evaluación final sin receta](17-evaluacion-final.md).

## Referencias

- [GnuCOBOL Manual](https://gnucobol.sourceforge.io/doc/gnucobol.html)
