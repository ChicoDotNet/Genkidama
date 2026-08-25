# Lección 02 — Modela dinero sin perder centavos

## Qué vas a conseguir

Crearás gastos válidos usando tipos, `final`, enums y validaciones explícitas de Dart.

## El problema

Guardar dinero como `double` puede introducir redondeos binarios difíciles de explicar. PocketLedger representa el monto en **centavos enteros** y sólo lo convierte a texto para mostrarlo.

## Concepto

`Expense` es un objeto inmutable para este alcance. Sus campos son `final`: después de construirlo no cambian. `ExpenseCategory` es un `enum` que evita strings libres como `"comida"`, `"Comida"` y `"food"` para la misma idea.

Dart usa null safety: si un campo no es nullable y es `required`, el compilador obliga a proporcionar un valor.

## Código real

Ver [`../app/lib/domain/expense.dart`](../app/lib/domain/expense.dart).

La construcción valida dos contratos:

- descripción no vacía después de `trim`;
- monto mayor que cero.

Un error de dominio no se esconde: se lanza `ArgumentError` con el parámetro que violó el contrato.

## Prueba

Ver [`../app/test/expense_test.dart`](../app/test/expense_test.dart).

[EJECUTAR]

```bash
flutter test test/expense_test.dart
```

Las pruebas usan una fecha fija. El reloj real pertenece a la frontera de aplicación; el modelo no debe consultar `DateTime.now()` por su cuenta.

## Tu turno

Añade una categoría `education` al enum y crea una prueba que construya un gasto válido de esa categoría. No cambies la UI todavía.

## Errores comunes

- Convertir dinero a `double` demasiado pronto.
- Guardar categorías como texto libre.
- Hacer que el modelo consulte reloj, archivos o red para construirse.
- Usar `String?` sólo para evitar pensar si el dato es realmente opcional.

## Siguiente paso

Continúa con [Lección 03 — Colecciones, estado y total de gastos](03-colecciones-estado-y-total.md).

## Referencias

- https://dart.dev/language/built-in-types
- https://dart.dev/language/null-safety
- https://dart.dev/language/enums
