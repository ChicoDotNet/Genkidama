# Lección 07 — Totales de control y reconciliación

## Qué vas a conseguir

Añadirás evidencia de control al batch: conteos e importes acumulados que permiten detectar si el resultado completo es coherente con los registros procesados.

## Antes de empezar

Completa la [Lección 06](06-file-status.md) y confirma que `bash tests/smoke.sh` está verde.

## El problema

Ver dos líneas correctas no demuestra que todo el batch sea correcto. Un proceso puede perder un registro, duplicarlo o calcular bien cada línea y aun así entregar un total final inconsistente. En procesamiento batch, los totales de control ayudan a reconciliar el conjunto, no sólo casos individuales.

## Concepto

NominaBatch conserva acumuladores separados para:

- registros procesados;
- registros rechazados;
- bruto total;
- deducciones totales;
- neto total.

Los importes sólo se acumulan después de que un registro supera validación y cálculo. Un rechazo incrementa su contador, pero nunca contamina los importes aceptados.

La relación más importante es:

```text
BRUTO - DEDUCCIONES = NETO
```

para el conjunto de registros aceptados, respetando la misma precisión decimal del cálculo individual.

## Demostración

[EN PANTALLA] Busca `WS-TOTALS` y `WRITE-SUMMARY` en [`../app/src/nomina.cob`](../app/src/nomina.cob).

Observa dónde ocurre cada `ADD`. La posición importa: acumular antes de terminar la validación introduciría datos rechazados en los controles.

## Código real

El reporte termina con una línea como:

```text
RESUMEN|PROCESADOS=2|RECHAZADOS=6|BRUTO=16317.50|DEDUCCIONES=1505.40|NETO=14812.10
```

Esa línea es fácil de leer y también fácil de verificar desde un script.

## Qué acaba de pasar

El batch ahora puede responder dos preguntas diferentes:

1. ¿Qué ocurrió con cada empleado?
2. ¿El conjunto procesado cuadra como unidad?

Los totales de control no sustituyen pruebas de reglas individuales; agregan una segunda capa de evidencia.

## Errores comunes

- acumular un registro antes de validar todos sus campos;
- usar campos de visualización para hacer aritmética;
- mezclar rechazados con importes aceptados;
- redondear de forma distinta el detalle y el resumen;
- verificar sólo el número de líneas y no los importes.

## Buenas prácticas

Haz aritmética en campos numéricos y formatea únicamente al escribir. Conserva una política de redondeo explícita. Prueba el total final además de valores individuales representativos.

Cuando agregues una nueva regla de cálculo, pregunta qué total de control podría detectar una regresión silenciosa.

## Tu turno

Calcula manualmente los totales esperados del fixture actual para `E001` y `E002`. Después identifica por qué `E003` y los registros inválidos adicionales no deben alterar esos importes.

## Cómo comprobar

Ejecuta:

```text
bash tests/smoke.sh
```

El smoke valida líneas individuales y el `RESUMEN` completo. Si cambia una regla legítimamente, actualiza primero la expectativa de negocio y luego la prueba; no relajes la aserción para conseguir verde.

## Solución enlazada

La implementación está en la aplicación canónica. El ejercicio integrador llega en el checkpoint 02 después de la siguiente lección.

## Reto adicional

¿Qué control agregarías para demostrar que ningún registro leído desapareció entre procesamiento y rechazo? Anota la invariante antes de mirar el checkpoint.

## Resumen

Los totales de control convierten una salida batch en un resultado reconciliable. Detalle y resumen protegen riesgos diferentes y se complementan.

## Siguiente paso

Continúa con la [Lección 08 — Pruebas de regresión y checkpoint 02](08-pruebas-regresion-y-checkpoint.md).

## Referencias

- [GnuCOBOL Manual](https://gnucobol.sourceforge.io/doc/gnucobol.html)
