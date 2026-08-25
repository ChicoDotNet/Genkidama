# Checkpoint 03 — Reporte mensual confiable

## Objetivo

Extiende PocketLedger para que el reporte mensual pueda mostrar cuál categoría concentra el mayor gasto del periodo sin mutar el ledger.

## Requisitos

1. Trabaja sobre `ExpenseReport`; no calcules el resultado dentro del widget.
2. Expón una propiedad nullable `largestCategory`.
3. Si no hay gastos, debe devolver `null`.
4. Si hay empate, documenta y prueba una regla determinista.
5. Añade al menos dos pruebas: sin gastos y con varias categorías.
6. Muestra el resultado en la tarjeta mensual sólo cuando exista.
7. Mantén dinero en centavos y colecciones no modificables.

## Evidencia

Desde `learn/es/dart/app`:

```bash
dart format --output=none --set-exit-if-changed lib test
flutter analyze
flutter test
```

No se acepta una solución que haga pasar el test modificando datos persistidos o que dependa del orden accidental de un `Map` sin documentarlo.

[Ver solución después de intentarlo](../solutions/checkpoint-03.md)
