# Lección 10 — Resumen derivado y capacidad visible

## Qué vas a conseguir

Vas a convertir una consulta en dos métricas simples y honestas: cantidad de citas visibles y minutos reservados.

## Antes de empezar

Completa la [Lección 09](09-consultas-temporales.md).

## El problema

Una lista ordenada responde “qué citas hay”, pero no responde rápidamente cuánto trabajo representa. Crear y persistir contadores separados introduce sincronización innecesaria.

## Concepto

Los datos derivados se recalculan desde la proyección autoritativa. `Schedule::bookedMinutes()` suma las duraciones del conjunto visible; `count($schedule->all())` produce el número de citas.

Una métrica derivada no debe fingir más precisión de la que tiene. “90 minutos reservados” no significa productividad, ingreso ni ocupación de toda la jornada.

## Demostración

[DEMO] Filtra un día con una cita de 60 minutos y otra de 30. La interfaz debe mostrar dos citas y 90 minutos. Filtra por un servicio que sólo coincide con una: el resumen debe cambiar junto con la tabla.

## Código real

[`Schedule::bookedMinutes()`](../app/src/Domain/Schedule.php) usa `array_map` y `array_sum`. No escribe ningún archivo. La plantilla sólo presenta el resultado y usa `aria-live="polite"` para que el resumen pueda anunciar cambios sin convertirlo en una alerta disruptiva.

## Qué acaba de pasar

La UI ofrece más información sin introducir estado duplicado ni una tabla de reportes. La misma proyección alimenta lista, conteo y minutos.

## Errores comunes

- Persistir un contador que puede reconstruirse barato.
- Calcular el resumen sobre todas las citas mientras la tabla está filtrada.
- Llamar “utilización” a minutos reservados sin conocer la capacidad disponible.
- Mezclar HTML dentro del dominio.

## Buenas prácticas

Nombra las métricas por lo que realmente miden. Si más adelante necesitas capacidad, ingresos o no-shows, agrega los datos que hagan esas métricas defendibles antes de mostrarlas.

## Tu turno

[PAUSA PARA EJERCICIO] Escribe una prueba con tres citas donde una consulta filtrada tenga 150 minutos y el calendario original 195. Explica qué defecto detecta la prueba.

## Cómo comprobar

```bash
composer test
```

Busca `testServiceFilterAndBookedMinutesAreDerivedFromProjection`.

## Solución enlazada

Compara después con [`ScheduleTest.php`](../app/tests/ScheduleTest.php).

## Reto adicional

Diseña, sin implementarla, una métrica “porcentaje ocupado”. Enumera qué datos adicionales necesitarías para que no sea engañosa.

## Resumen

- Los resúmenes derivables no necesitan persistencia propia.
- La tabla y el resumen deben usar la misma proyección.
- Una métrica se nombra según lo que realmente demuestra.

## Siguiente paso

La [Lección 11](11-exportar-csv-como-frontera.md) convertirá esa misma consulta en un archivo interoperable.

## Referencias

- [array_sum — PHP](https://www.php.net/manual/en/function.array-sum.php)
- [array_map — PHP](https://www.php.net/manual/en/function.array-map.php)
- [WAI-ARIA live regions — W3C](https://www.w3.org/WAI/WCAG22/Techniques/aria/ARIA22)
