# Lección 15 — Diagnóstico y rendimiento con evidencia

## Qué vas a conseguir

Aprenderás a diagnosticar un batch por sus contratos observables y a razonar sobre rendimiento sin optimizar por intuición.

## Antes de empezar

Completa la [Lección 14](14-tooling-y-gate-profesional.md) y deja `bash tools/verify.sh` verde.

## El problema

Un batch que devuelve “algo salió mal” obliga a adivinar. Y un batch que “parece lento” invita a reemplazar estructuras antes de saber qué cuesta realmente.

## Concepto

NominaBatch ya expone señales útiles: `FILE STATUS`, códigos de retorno diferenciados, contadores de procesados/rechazados y totales reconciliables. Esas señales permiten reducir el problema antes de abrir el código.

En rendimiento, la búsqueda de duplicados es lineal sobre una tabla limitada a 100 IDs. En el peor caso del lote actual se hacen comparaciones del orden de `n²`, pero con `n <= 100` la claridad domina. Si el requisito cambia a millones de filas, primero se mide volumen, tiempo y memoria; después se cambia la estructura.

## Demostración

[DEMO] Ejecuta:

```bash
bash tests/operational.sh
```

Observa que una entrada ausente termina con código `2` y `ERROR|EMPLOYEE_OPEN|STATUS=...`, mientras un reporte imposible de abrir termina con código `3`.

Después ejecuta:

```bash
time bash tests/smoke.sh
```

El tiempo local es una observación, no un benchmark contractual. Repite varias veces antes de concluir nada.

## Código real

`FIND-DUPLICATE-ID` recorre como máximo `WS-SEEN-COUNT` posiciones. La capacidad explícita de 100 convierte una complejidad potencialmente creciente en un costo acotado por el contrato actual.

Los totales y bandas ofrecen otra forma de diagnóstico: si el número de aceptados o los netos dejan de reconciliar, existe un defecto de estado aunque el proceso termine con código cero.

## Qué acaba de pasar

Separaste dos preguntas distintas: “¿por qué falló?” y “¿qué parte cuesta?”. La primera usa diagnósticos y contratos; la segunda requiere medición representativa.

## Errores comunes

- tratar `DISPLAY` abundante como observabilidad;
- optimizar la búsqueda de 100 elementos sin evidencia;
- usar una sola ejecución de `time` como benchmark;
- confundir código de retorno con causa suficiente si el mensaje no aporta contexto.

## Buenas prácticas

Mantén diagnósticos estables y concisos. Mide con fixtures representativos y conserva pruebas funcionales durante cualquier optimización.

## Tu turno

Explica qué tres métricas observarías si el límite subiera de 100 a 100 000 registros y qué hipótesis intentarías confirmar antes de sustituir la búsqueda lineal.

## Cómo comprobar

Tu respuesta debe distinguir volumen de entrada, tiempo de procesamiento y uso de memoria, y debe preservar la regla de duplicados como comportamiento independientemente de la implementación.

## Solución enlazada

No necesitas código nuevo para una respuesta válida: el objetivo es formular una hipótesis medible antes de cambiar la estructura.

## Reto adicional

Diseña un benchmark reproducible que genere datos ficticios, pero no lo conviertas en gate de CI hasta controlar variabilidad y umbrales.

## Resumen

Diagnosticar es reducir incertidumbre; optimizar es responder a evidencia, no a estética.

## Siguiente paso

Continúa con la [Lección 16 — operación confiable y checkpoint 04](16-operacion-confiable-y-checkpoint-04.md).

## Referencias

- [GnuCOBOL Manual](https://gnucobol.sourceforge.io/doc/gnucobol.html)
