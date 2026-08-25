# Solución de referencia — Evaluación final PocketLedger

Esta referencia no es la única solución válida. Úsala para comparar fronteras, pruebas y trade-offs después de intentar la evaluación.

## 1. Presupuesto por categoría

Una solución proporcionada mantiene el presupuesto como dato de entrada de una regla pura, por ejemplo un pequeño objeto de dominio o función que recibe `ExpenseReport`, categoría y `budgetCents`. El resultado puede exponer `spentCents`, `budgetCents`, `remainingCents` y `isOverBudget`.

No uses `double` para almacenar dinero ni leas estado de widgets desde la regla.

## 2. Semántica temporal

Define primero qué significa `spentAt`. Una opción coherente es persistir instantes UTC y convertirlos a local antes de decidir el mes visible de la persona usuaria. La corrección debe vivir en una frontera única —por ejemplo al construir el reporte— y la prueba debe incluir un instante cercano a medianoche que cruce un límite de mes en la zona local usada por el test.

Lo importante es no mezclar comparaciones UTC y local accidentalmente.

## 3. Persistencia con contexto

`ExpenseStoreException` puede conservar una causa técnica y añadir una operación estable como `load` o `save`, sin incorporar contenido del gasto. El controlador continúa mostrando un mensaje útil y no sustituye datos corruptos por `[]`.

Una prueba debe demostrar que un fallo de escritura deja el ledger visible sin cambios.

## 4. UI y prueba de widget

La UI deriva el estado de presupuesto de la misma fuente que el reporte. La prueba crea fixtures deterministas, monta PocketLedger y verifica una señal visible tanto bajo como sobre el límite. Evita comprobar detalles de implementación internos del widget.

## 5. Diagnóstico agregado

Añade únicamente agregados, por ejemplo cantidad de categorías excedidas. No copies descripciones, nombres de comercios ni valores arbitrarios introducidos por la persona usuaria a `ExpenseDiagnostics`.

## 6. Frontera futura de persistencia

El objetivo no es construir una nube. Una interfaz pequeña puede expresar únicamente las capacidades que la aplicación ya necesita: cargar y guardar una colección validada. La implementación local existente satisface ese contrato y una futura implementación remota podría hacerlo después.

No agregues autenticación, HTTP, sincronización bidireccional ni resolución de conflictos hasta que exista un requerimiento real.

## Pruebas mínimas de referencia

- presupuesto dentro y fuera del límite;
- fecha que cruza fin/inicio de mes;
- fallo de persistencia conserva estado visible;
- widget muestra la señal correcta;
- diagnóstico no contiene texto sensible.

Ejecuta:

```bash
flutter pub get
dart format lib test
flutter analyze
flutter test
flutter build web --release
```

## Qué defender en una revisión

Explica por qué separaste dominio, estado, persistencia y UI; cómo evitaste pérdida silenciosa; qué evidencia protege el bugfix; y por qué una frontera preparada para crecer es preferible a implementar infraestructura futura sin necesidad presente.
