# Solución de referencia — Checkpoint 02

Una solución proporcionada agrega una identidad inmutable al gasto y mantiene la misma regla transaccional de `addExpense()`: **persistir antes de publicar**.

## Enfoque

1. Añade `id` a `Expense` y persístelo en `toJson()` / `fromJson()`.
2. Genera el identificador en la frontera de aplicación al crear el gasto; para el ejercicio basta una estrategia determinista/injectable en tests.
3. En `ExpenseController`, construye una lista candidata sin el gasto solicitado.
4. Si el identificador no existe, devuelve un error explícito en vez de fingir éxito.
5. Ejecuta `_store.save(candidate)`.
6. Sólo después reemplaza `_ledger` y llama `notifyListeners()`.

Una forma de la operación es:

```dart
Future<void> removeExpense(String id) async {
  final candidate = expenses.where((expense) => expense.id != id).toList();
  if (candidate.length == expenses.length) {
    throw ArgumentError.value(id, 'id', 'No existe ese gasto.');
  }

  await _store.save(candidate);
  _ledger = ExpenseLedger(candidate);
  notifyListeners();
}
```

La UI no elimina de una lista. Solicita la operación, espera el resultado y muestra un error si la persistencia falla.

## Prueba importante

Configura `MemoryExpenseStore(failSave: true)`, intenta eliminar y comprueba dos cosas:

- se propaga `ExpenseStoreException`;
- el gasto original continúa en `controller.expenses`.

Esa prueba protege el contrato que importa más que la implementación exacta de botones o diálogos.

## Trade-off

Un UUID real sería razonable en una aplicación posterior, pero el checkpoint no necesita agregar un paquete sólo para demostrar consistencia. Lo importante aquí es que la identidad sea estable y que los tests puedan controlarla.
