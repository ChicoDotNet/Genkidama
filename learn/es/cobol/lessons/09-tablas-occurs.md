# Lección 09 — Tablas `OCCURS` para resumir el lote

## Qué vas a conseguir

Aprenderás a representar un conjunto pequeño y de tamaño fijo con `OCCURS` y lo usarás para agregar resultados reales de NominaBatch por banda de deducción.

## Antes de empezar

Completa la [Lección 08](08-pruebas-regresion-y-checkpoint.md) y confirma desde `app/`:

```text
bash tests/smoke.sh
```

## El problema

El reporte global dice cuánto bruto, deducción y neto produjo el lote, pero no permite responder una pregunta operativa sencilla: ¿cómo se distribuye el neto entre empleados con deducción cero, hasta 10%, hasta 20% y más de 20%?

Podríamos crear cuatro variables independientes, pero esa solución duplica estructura y vuelve torpe cualquier operación que deba aplicarse a todas las categorías.

## Concepto

`OCCURS` declara elementos repetidos dentro de una estructura COBOL. Para una cantidad conocida y pequeña de categorías es una herramienta natural: cada elemento conserva los mismos campos y se accede mediante subíndice.

NominaBatch agrega cuatro bandas:

```text
01  WS-DEDUCTION-BANDS.
    05 WS-BAND OCCURS 4 TIMES.
        10 WS-BAND-NAME  PIC X(12) VALUE SPACES.
        10 WS-BAND-COUNT PIC 9(6) VALUE ZERO.
        10 WS-BAND-NET   PIC 9(10)V99 VALUE ZERO.
```

El número `4` es parte del contrato de esta versión: las categorías son deliberadamente fijas y explícitas. No usamos una tabla porque "suene empresarial", sino porque realmente necesitamos repetir el mismo esquema.

## Demostración

[DEMO] Abre [`../app/src/nomina.cob`](../app/src/nomina.cob) y localiza `INITIALIZE-BANDS` y `ACCUMULATE-BAND`.

La clasificación usa `EVALUATE TRUE`:

```text
EVALUATE TRUE
    WHEN WS-DEDUCTION-PCT = 0
        MOVE 1 TO WS-BAND-NUMBER
    WHEN WS-DEDUCTION-PCT <= 10
        MOVE 2 TO WS-BAND-NUMBER
    WHEN WS-DEDUCTION-PCT <= 20
        MOVE 3 TO WS-BAND-NUMBER
    WHEN OTHER
        MOVE 4 TO WS-BAND-NUMBER
END-EVALUATE
```

Después sólo actualizamos el elemento elegido:

```text
ADD 1 TO WS-BAND-COUNT(WS-BAND-NUMBER)
ADD WS-NET TO WS-BAND-NET(WS-BAND-NUMBER)
```

## Código real

El registro aceptado `E009` usa 25% de deducción. Su neto pertenece a `MAS20`. Los registros `E001` y `E002` pertenecen a `HASTA10`.

El fixture sigue siendo ficticio; está diseñado para que la prueba pueda comprobar varias categorías sin depender de información personal.

## Qué acaba de pasar

Pasamos de variables aisladas a una estructura repetible. La regla de negocio sigue visible: cuatro bandas concretas, clasificación determinista y acumulación sólo después de que el registro fue validado.

## Errores comunes

- usar `OCCURS` para una sola cosa que nunca se recorre ni se indexa;
- olvidar que los subíndices COBOL parten de 1;
- escribir fuera del tamaño declarado;
- actualizar agregados antes de terminar todas las validaciones;
- mezclar el nombre visible de una banda con la regla que decide a cuál pertenece;
- asumir que una tabla en memoria equivale a una base de datos.

## Buenas prácticas

Mantén pequeño el contrato de la tabla. Documenta qué significa cada posición. Si el tamaño deja de ser razonablemente fijo o los datos deben sobrevivir entre ejecuciones, reevalúa la estructura en lugar de aumentar `OCCURS` indefinidamente.

Para datos monetarios conserva `PIC` decimal explícito; no introduzcas representación binaria aproximada para importes sólo porque estés trabajando con una tabla.

## Tu turno

Agrega temporalmente un cuarto registro válido con 15% de deducción y predice qué banda debe incrementar antes de ejecutar el batch. Después comprueba el reporte y revierte el fixture para conservar el estado canónico.

## Cómo comprobar

```text
bash tests/smoke.sh
```

La prueba verifica los netos agregados de las cuatro bandas.

## Solución enlazada

No hay solución independiente para este ejercicio pequeño: compara tu comportamiento con el reporte canónico y el código de la aplicación.

## Reto adicional

Explica qué cambiarías si las bandas vinieran de configuración externa. No lo implementes todavía: identifica qué parte del diseño dejaría de ser un contrato fijo.

## Resumen

`OCCURS` permite modelar colecciones pequeñas y homogéneas. En NominaBatch resuelve una necesidad visible del reporte sin introducir persistencia ni abstracciones innecesarias.

## Siguiente paso

Continúa con la [Lección 10 — Recorrer tablas con `PERFORM VARYING`](10-perform-varying-y-reportes.md).

## Referencias

- [GnuCOBOL Manual](https://gnucobol.sourceforge.io/doc/gnucobol.html)
