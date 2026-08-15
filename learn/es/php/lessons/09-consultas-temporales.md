# Lección 09 — Consultas temporales sin duplicar estado

## Qué vas a conseguir

Vas a consultar AgendaPHP por día y por texto de servicio sin crear una segunda colección persistida ni mezclar filtros con la escritura del calendario.

## Antes de empezar

Completa la [Lección 08](08-ciclo-de-vida-y-checkpoint-02.md).

## El problema

Una agenda con pocas citas puede mostrarse completa. En cuanto crece, la pregunta cambia: “¿qué tengo el jueves?” o “¿cuáles son las consultas fiscales?”. Copiar resultados filtrados a otro archivo parece fácil, pero crea dos fuentes de verdad.

## Concepto

Una **proyección** se calcula desde el estado autoritativo. `Schedule::between()` y `Schedule::matchingService()` devuelven nuevos calendarios derivados sin modificar el original.

El rango temporal usa semántica semiabierta `[inicio, fin)`: incluye la medianoche inicial y excluye la siguiente. Así una cita exactamente a las 00:00 pertenece a un solo día.

## Demostración

[DEMO] Crea citas el 20 y 21 de agosto. Filtra `2026-08-20`: sólo deben aparecer las del día 20. Después agrega `service=consulta` y confirma que ambos filtros se componen.

## Código real

Revisa [`Schedule.php`](../app/src/Domain/Schedule.php). `between()` y `matchingService()` usan `array_filter`, pero conservan la regla importante: el calendario original no cambia.

La frontera HTTP de [`public/index.php`](../app/public/index.php) convierte `YYYY-MM-DD` a `DateTimeImmutable` usando la zona horaria configurada. Un valor imposible como `2026-02-31` devuelve 422 en lugar de convertirse silenciosamente.

## Qué acaba de pasar

Separaste **estado** de **consulta**. JSON sigue almacenando exactamente las citas; los filtros se reconstruyen cuando el usuario los solicita.

## Errores comunes

- Persistir “agenda del día” como otra colección.
- Comparar fechas como strings con formatos incompatibles.
- Usar un rango cerrado en ambos extremos y contar medianoche dos veces.
- Mutar el calendario original al filtrar.
- Corregir fechas inválidas automáticamente sin informar al usuario.

## Buenas prácticas

Haz deterministas las consultas y explícitos los límites temporales. El almacenamiento no necesita conocer cada filtro de UI mientras el volumen permita proyectar en memoria de forma razonable.

## Tu turno

[PAUSA PARA EJERCICIO] Agrega tres citas repartidas en dos días y escribe una prueba que combine `between()` con `matchingService()`. Comprueba además que `schedule->all()` sigue conteniendo las tres.

## Cómo comprobar

```bash
cd app
composer test
bash tools/smoke.sh
```

El smoke prueba un día con resultado y otro vacío.

## Solución enlazada

Este ejercicio se integra en el código canónico; compara tu solución con las pruebas de [`ScheduleTest.php`](../app/tests/ScheduleTest.php) después de intentarlo.

## Reto adicional

Explica cómo cambiaría la consulta si una cita de 90 minutos empieza a las 23:30 y quieres buscar por **intervalos que se cruzan con el día**, no sólo por hora de inicio.

## Resumen

- Los filtros son proyecciones, no estado nuevo.
- `[inicio, fin)` evita fronteras ambiguas.
- La zona horaria se interpreta en la frontera.
- Filtrar no modifica el calendario durable.

## Siguiente paso

La [Lección 10](10-resumen-derivado-y-capacidad.md) convertirá la proyección en información útil sin inventar otra base de datos.

## Referencias

- [DateTimeImmutable — PHP](https://www.php.net/manual/en/class.datetimeimmutable.php)
- [array_filter — PHP](https://www.php.net/manual/en/function.array-filter.php)
- [Date and Time — PHP](https://www.php.net/manual/en/book.datetime.php)
