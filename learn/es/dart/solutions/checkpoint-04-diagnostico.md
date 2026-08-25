# Solución de referencia — Checkpoint 04

Una solución suficiente carga fixtures mediante el store de memoria, ejecuta `load()` y obtiene un único snapshot con `diagnostics()`.

La prueba de referencia debe comprobar al menos:

```dart
final diagnostics = controller.diagnostics();
expect(diagnostics.expenseCount, 3);
expect(diagnostics.totalCents, expectedTotal);
expect(diagnostics.categoryCounts[ExpenseCategory.food], 2);
expect(diagnostics.categoryCounts[ExpenseCategory.transport], 1);
expect(diagnostics.toString(), isNot(contains('descripción sensible')));
```

La idea importante no es el texto exacto del fixture. Es demostrar que el contrato de diagnóstico ofrece evidencia operacional sin transportar descripciones del dominio.

Después ejecuta:

```bash
cd learn/es/dart/app
dart format lib test
flutter analyze
flutter test
flutter build web --release
```

Si no tienes una plataforma Flutter capaz de construir el target elegido, documenta esa limitación en vez de presentar una validación que no ejecutaste.

## Por qué esta solución

- deriva información del ledger actual;
- no mantiene contadores paralelos;
- mantiene los centavos enteros;
- no altera persistencia;
- protege privacidad por diseño;
- convierte el diagnóstico en un contrato ejecutable.
