# Lección 08 — Pruebas de regresión y checkpoint 02

## Qué vas a conseguir

Convertirás reglas y fallos conocidos de NominaBatch en pruebas repetibles que protegen tanto el camino feliz como entradas inválidas representativas.

## Antes de empezar

Completa la [Lección 07](07-totales-control.md). Desde `app/`, ejecuta el smoke actual antes de modificar cualquier expectativa.

## El problema

Una prueba que sólo confirma dos empleados válidos puede pasar aunque se rompa la validación. En un batch de negocio, una regresión peligrosa puede aceptar horas no numéricas, tarifa cero, deducciones imposibles o registros incompletos y producir cifras aparentemente razonables.

## Concepto

Una prueba de regresión conserva un comportamiento que ya decidiste que debe permanecer cierto. No busca cubrir líneas por sí misma; debe detectar un defecto real.

El fixture canónico contiene casos válidos e inválidos deliberados. `tests/smoke.sh` compila desde cero, ejecuta el batch y comprueba dos cálculos aceptados, horas fuera de rango, formato incompleto, horas no numéricas, tarifa cero, deducción mayor a 100%, ID vacío y el resumen con conteos e importes exactos.

## Demostración

[EN PANTALLA] Compara [`../app/data/employees.dat`](../app/data/employees.dat) con [`../app/tests/smoke.sh`](../app/tests/smoke.sh).

Cada línea inválida existe por una razón concreta. Si una deja de rechazarse, la prueba debe fallar con evidencia específica del contrato roto.

## Código real

El smoke usa comparaciones literales intencionalmente pequeñas:

```text
grep -F "RECHAZADO|E005|HORAS no es numérico" report.txt
```

No es un framework de testing; para este punto del curso es una barrera ligera, portable y suficiente para validar el ejecutable completo desde entrada hasta reporte.

## Qué acaba de pasar

La aplicación ya no depende de revisión manual para conservar sus validaciones principales. El mismo comando sirve a una persona y al workflow de GitHub Actions.

## Errores comunes

- probar sólo el camino feliz;
- hacer una prueba tan genérica que no identifica qué contrato falló;
- duplicar casos sin riesgo distinto sólo para aumentar cantidad;
- editar la prueba para acomodar un bug de producción;
- usar datos reales o información personal en fixtures educativos;
- afirmar cobertura porcentual cuando no existe una medición real.

## Buenas prácticas

Cada caso debe responder: qué defecto detectaría. Mantén fixtures ficticios, deterministas y pequeños. Compila dentro del test para que una copia limpia no dependa de binarios previos.

En este curso todavía no medimos cobertura de líneas con una herramienta COBOL; por eso reportamos comportamiento validado, no un porcentaje inventado.

## Tu turno

Añade un registro inválido nuevo que pruebe una frontera distinta sin cambiar los totales aceptados. Antes de escribirlo, explica qué defecto detectaría y cuál debe ser el mensaje de rechazo.

## Cómo comprobar

```text
bash tests/smoke.sh
```

Debes observar `NominaBatch smoke: OK`. Después revisa `report.txt` y confirma que el resumen sólo suma registros aceptados.

## Solución enlazada

Ahora resuelve el [Checkpoint 02](../exercises/checkpoint-02.md) sin abrir la [solución de referencia](../solutions/checkpoint-02.md).

## Reto adicional

Diseña una prueba de integración adicional para un fallo de apertura usando una copia aislada del laboratorio. Explica qué señal debería observar la prueba y por qué el fixture principal debe permanecer estable.

## Resumen

Las pruebas de regresión protegen decisiones concretas. Un batch junior defendible debe demostrar resultados correctos y rechazos correctos, no sólo compilar.

## Siguiente paso

El siguiente bloque introducirá estructuras COBOL para procesar y consultar conjuntos pequeños dentro del batch, empezando por tablas con `OCCURS`.

## Referencias

- [GnuCOBOL](https://gnucobol.sourceforge.io/)
- [GnuCOBOL Manual](https://gnucobol.sourceforge.io/doc/gnucobol.html)
