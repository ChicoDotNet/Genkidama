# Checkpoint 02 — Elimina un gasto sin perder consistencia

PocketLedger ya tiene una frontera de persistencia y un controlador que guarda antes de publicar cambios. Recupera esas ideas sin receta paso a paso.

## Encargo

Permite eliminar un gasto desde la aplicación manteniendo disco y memoria de acuerdo.

Requisitos:

1. Define una identidad estable para cada `Expense`; no uses el índice visual como identidad persistente.
2. Agrega una operación de eliminación al estado de aplicación, no al widget como manipulación directa de una lista.
3. Persiste el snapshot candidato antes de publicarlo en memoria.
4. Si `save()` falla, el gasto debe seguir visible.
5. La UI debe pedir confirmación antes de eliminar.
6. Añade al menos una prueba del controlador y una prueba de widget.
7. Conserva verdes formatter, analyzer y toda la suite existente.

## Evidencia

Desde `app/`:

```bash
dart format --output=none --set-exit-if-changed lib test
flutter analyze
flutter test
```

Después provoca un `failSave` en el store de prueba: la eliminación debe fallar sin cambiar el ledger visible.

## Reto adicional

Diseña la identidad para que una futura importación pueda detectar duplicados sin depender de una base de datos.

## Después de intentarlo

Compara tu enfoque con la [solución de referencia](../solutions/checkpoint-02.md).
