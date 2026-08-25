# Solución de referencia — Checkpoint 01

Una solución proporcionada conserva la lógica de acumulación dentro de `ExpenseLedger` y deja a la UI sólo la responsabilidad de representar el resultado.

En `ExpenseHomePage.build`, obtiene una vez:

```dart
final categoryTotals = _ledger.totalsByCategory();
```

Después del card de total general puedes renderizar entradas como:

```dart
...categoryTotals.entries.map(
  (entry) => ListTile(
    dense: true,
    title: Text(entry.key.name),
    trailing: Text(_money(entry.value)),
  ),
),
```

La prueba de widget debería registrar dos gastos en categorías distintas y comprobar ambos nombres y montos. Lo importante no es copiar exactamente este layout: el contrato es que la UI derive el resumen desde la frontera de dominio ya existente y que una prueba visible lo proteja.

Para el reto adicional, crea una lista local de `categoryTotals.entries`, ordénala por `value` descendente y renderiza esa lista. No ordenes ni reescribas `_ledger.expenses`: son dos contratos diferentes.

## Trade-off

En este punto `setState` y un cálculo pequeño son suficientes. Memorizar un paquete externo de estado para una pantalla sería complejidad accidental. Cuando PocketLedger incorpore persistencia y más pantallas, reevaluaremos la frontera con evidencia del problema real.
