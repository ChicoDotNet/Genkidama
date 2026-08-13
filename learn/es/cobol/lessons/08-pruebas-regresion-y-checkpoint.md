# Lección 08 — Pruebas de regresión y checkpoint 02

## Qué vas a conseguir

Convertirás reglas y fallos conocidos de NominaBatch en pruebas repetibles que protegen el camino feliz y entradas inválidas representativas.

## Antes de empezar

Completa la [Lección 07](07-totales-control.md). Desde `app/`, ejecuta el smoke actual antes de modificar expectativas.

## El problema

Una prueba que sólo confirma empleados válidos puede pasar aunque se rompa la validación. Un batch puede aceptar horas no numéricas, tarifa cero, deducciones imposibles o registros incompletos y producir cifras aparentemente razonables.

## Concepto

Una prueba de regresión conserva un comportamiento decidido. El fixture canónico contiene casos válidos e inválidos deliberados. `tests/smoke.sh` compila desde cero, ejecuta el batch y comprueba cálculos, rechazos y resumen exacto.

## Demostración

[EN PANTALLA] Compara [`../app/data/employees.dat`](../app/data/employees.dat) con [`../app/tests/smoke.sh`](../app/tests/smoke.sh). Cada línea inválida debe corresponder a un contrato verificable.

## Código real

El smoke usa comparaciones literales pequeñas, por ejemplo:

```text
grep -F "RECHAZADO|E005|HORAS no es numérico" report.txt
```

No es un framework de testing; aquí funciona como barrera ligera de punta a punta.

## Qué acaba de pasar

La aplicación ya no depende de revisión manual para conservar sus validaciones principales. El mismo comando sirve localmente y en GitHub Actions.

## Errores comunes

- probar sólo el camino feliz;
- hacer una prueba tan genérica que no identifica el contrato roto;
- duplicar casos sin riesgo distinto;
- editar la prueba para acomodar un bug;
- usar datos reales en fixtures;
- afirmar cobertura porcentual sin medición.

## Buenas prácticas

Cada caso debe responder qué defecto detectaría. Mantén fixtures ficticios, deterministas y pequeños. Compila dentro del test para que una copia limpia no dependa de binarios previos.

## Tu turno

Añade temporalmente un registro inválido que pruebe una frontera distinta y predice el rechazo antes de ejecutar.

## Cómo comprobar

```text
bash tests/smoke.sh
```

Debes observar `NominaBatch smoke: OK`.

## Solución enlazada

Resuelve el [Checkpoint 02](../exercises/checkpoint-02.md) sin abrir la [solución de referencia](../solutions/checkpoint-02.md).

## Reto adicional

Diseña una prueba aislada para un fallo de apertura y explica qué señal observarías.

## Resumen

Las pruebas de regresión protegen decisiones concretas; un batch defendible demuestra resultados correctos y rechazos correctos.

## Siguiente paso

Continúa con la [Lección 09 — Tablas `OCCURS` para resumir el lote](09-tablas-occurs.md).

## Referencias

- [GnuCOBOL](https://gnucobol.sourceforge.io/)
- [GnuCOBOL Manual](https://gnucobol.sourceforge.io/doc/gnucobol.html)
