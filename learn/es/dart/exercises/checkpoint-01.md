# Checkpoint 01 — Resumen por categoría

Hasta aquí PocketLedger ya puede crear gastos, conservarlos en un ledger y actualizar el total visible. Ahora recupera esas ideas sin una receta paso a paso.

## Encargo

Añade debajo del total general un resumen que muestre cuánto se ha gastado en cada categoría que tenga al menos un gasto.

Requisitos:

1. Usa `ExpenseLedger.totalsByCategory()`; no recalcules los totales recorriendo widgets.
2. Muestra únicamente categorías con gasto registrado.
3. El monto visible debe usar dos decimales.
4. Añade una prueba de widget con al menos dos categorías.
5. Conserva verdes las pruebas existentes.
6. No agregues un paquete de gestión de estado para resolver este checkpoint.

## Evidencia

Desde `app/`:

```bash
flutter analyze
flutter test
```

La prueba nueva debe fallar si eliminas el resumen de la UI.

## Reto adicional

Ordena el resumen por mayor gasto sin modificar el orden de inserción de `ExpenseLedger.expenses`.

## Después de intentarlo

Compara con la [solución de referencia](../solutions/checkpoint-01.md).
