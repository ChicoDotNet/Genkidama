# Lección 11 — Reporte visible con la misma fuente de verdad

## Qué vas a conseguir

La pantalla principal mostrará el gasto del mes actual y permitirá filtrar la lista por categoría sin guardar estado duplicado.

## El problema

Es tentador recalcular y almacenar en variables separadas el total mensual, la lista filtrada y el total global. Eso obliga a mantener varias copias sincronizadas.

## Código real

En [`../app/lib/application/expense_controller.dart`](../app/lib/application/expense_controller.dart), `reportForMonth` y `expensesForCategory` derivan información del mismo `ExpenseLedger`.

En [`../app/lib/main.dart`](../app/lib/main.dart), el widget conserva sólo `_categoryFilter`, que sí es estado de presentación. El reporte mensual y la lista visible se recalculan desde el controlador durante `build`.

La tarjeta mensual utiliza la clave `month-report` y el selector `category-filter`; las pruebas de widget validan tanto el resumen como el filtrado.

## Tu turno

Agrega un tercer gasto de otra categoría al test de widget y comprueba que el reporte mensual incluya los tres aunque el filtro visual muestre sólo uno.

## Buenas prácticas

- Mantén datos persistidos en una sola frontera.
- Trata filtros como estado de UI, no como una copia del dominio.
- Prueba el resultado que ve la persona y también el cálculo puro cuando tenga reglas propias.

## Siguiente paso

En la lección 12 haremos que un fallo de carga sea recuperable mediante un reintento explícito.

## Referencias

- https://docs.flutter.dev/data-and-backend/state-mgmt/simple
- https://docs.flutter.dev/testing/widget-tests

[Siguiente: Lección 12 — recuperación explícita](12-recuperacion-explicita-tras-un-fallo.md)
