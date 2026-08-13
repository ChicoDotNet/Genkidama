# Lección 10 — Recorrer tablas con `PERFORM VARYING`

## Qué vas a conseguir

Recorrerás una tabla `OCCURS` de forma explícita y generarás una línea de reporte por cada banda de deducción.

## Antes de empezar

Completa la [Lección 09](09-tablas-occurs.md).

## El problema

Ya acumulamos datos por banda, pero escribir cuatro bloques casi idénticos para reportarlos duplicaría lógica y haría fácil olvidar una categoría.

## Concepto

`PERFORM VARYING` expresa una iteración controlada. En NominaBatch recorremos posiciones 1 a 4:

```text
PERFORM VARYING WS-BAND-LOOP FROM 1 BY 1
    UNTIL WS-BAND-LOOP > 4
    ...
END-PERFORM
```

El contador sólo decide qué elemento leer. La tabla sigue siendo la fuente de nombres, conteos y netos.

## Demostración

[DEMO] Revisa `WRITE-BAND-SUMMARIES` en [`../app/src/nomina.cob`](../app/src/nomina.cob). Observa cómo el mismo bloque formatea cada posición y cómo los `PIC` de display mantienen separada la representación del valor numérico.

## Código real

El reporte canónico termina con líneas como:

```text
BANDA|HASTA10|EMPLEADOS=2|NETO=14812.10
BANDA|MAS20|EMPLEADOS=1|NETO=6000.00
```

Las bandas vacías también se escriben. Eso vuelve explícito el contrato del reporte y facilita detectar que una categoría existe aunque tenga cero empleados.

## Qué acaba de pasar

La tabla ahora sirve tanto para acumular como para producir salida uniforme. Eliminamos repetición sin esconder la regla de negocio.

## Errores comunes

- iniciar el subíndice en 0;
- usar un límite distinto del tamaño real de la tabla;
- alterar el contador dentro del cuerpo sin necesidad;
- mezclar acumulación y presentación en el mismo loop;
- omitir categorías vacías y cambiar silenciosamente el contrato del reporte.

## Buenas prácticas

Mantén límites visibles y coherentes. Si el tamaño deja de ser fijo, centraliza ese límite antes de duplicarlo en múltiples párrafos. Usa nombres de trabajo que indiquen si una variable es subíndice, contador o dato de negocio.

## Tu turno

Agrega una línea temporal de diagnóstico con el número de posición y comprueba el orden 1, 2, 3, 4. Después retírala: el reporte final no necesita detalles internos.

## Cómo comprobar

```text
bash tests/smoke.sh
```

El smoke exige las cuatro líneas `BANDA` en el reporte.

## Solución enlazada

No hay solución separada para este ejercicio breve.

## Reto adicional

Explica cuándo preferirías un índice COBOL frente a un subíndice numérico. No cambies el código sólo por usar una característica distinta.

## Resumen

`PERFORM VARYING` permite aplicar una operación uniforme a una tabla `OCCURS`. La iteración existe para reducir duplicación y hacer el reporte más consistente.

## Siguiente paso

Continúa con la [Lección 11 — Buscar IDs y proteger la integridad del lote](11-busqueda-ids-duplicados.md).

## Referencias

- [GnuCOBOL Manual](https://gnucobol.sourceforge.io/doc/gnucobol.html)
