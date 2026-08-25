# Lección 03 — Colecciones, estado y total de gastos

## Qué vas a conseguir

Mantendrás una colección de gastos sin exponer su lista mutable y calcularás totales con operaciones de colección.

## El problema

Si cualquier widget pudiera modificar directamente una `List<Expense>`, el estado quedaría repartido y las pruebas no sabrían qué frontera protege las reglas. `ExpenseLedger` concentra esa responsabilidad.

## Concepto

El ledger conserva una lista privada y expone `UnmodifiableListView`. Eso permite leer sin entregar la misma referencia mutable al exterior.

`fold` reduce la colección a un total; `Map.update` permite acumular por categoría. Estas operaciones son más importantes que memorizar sintaxis aislada porque aparecen constantemente en código Dart real.

## Código real

- [`../app/lib/domain/expense_ledger.dart`](../app/lib/domain/expense_ledger.dart)
- [`../app/test/expense_ledger_test.dart`](../app/test/expense_ledger_test.dart)

[EJECUTAR]

```bash
flutter test test/expense_ledger_test.dart
```

## Estado en Flutter

`ExpenseHomePage` es `StatefulWidget` porque la lista visible cambia durante la vida de la pantalla. En este primer incremento, `setState` es suficiente: no necesitamos introducir un paquete de gestión de estado antes de que exista presión real.

Cuando se agrega un gasto, la UI vuelve a construir las partes afectadas y lee el total desde el ledger.

## Tu turno

Añade una prueba con tres gastos de dos categorías y comprueba `totalsByCategory()`. No pruebes el orden de un `Map` si el contrato no lo necesita.

## Buenas prácticas

- Mantén el estado mutable detrás de una frontera clara.
- Devuelve vistas o copias cuando el caller no deba mutar internamente.
- No introduzcas Provider, Riverpod, Bloc u otra biblioteca sólo por exhibición; llegará una abstracción cuando el problema la justifique.

## Siguiente paso

Continúa con [Lección 04 — Formulario, errores y pruebas de widget](04-formulario-errores-y-pruebas.md).

## Referencias

- https://api.dart.dev/dart-collection/UnmodifiableListView-class.html
- https://docs.flutter.dev/data-and-backend/state-mgmt/ephemeral-vs-app
