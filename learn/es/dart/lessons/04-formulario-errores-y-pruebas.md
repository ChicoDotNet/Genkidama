# Lección 04 — Formulario, errores y pruebas de widget

## Qué vas a conseguir

Registrarás un gasto desde la UI, convertirás entrada textual en un objeto válido y probarás el flujo visible sin depender de taps manuales.

## El problema

`TextField` entrega texto. El dominio exige descripción no vacía y centavos positivos. La frontera de UI debe traducir entrada → tipos y mostrar un error útil cuando no puede hacerlo.

## Código real

Abre [`../app/lib/main.dart`](../app/lib/main.dart). El diálogo usa campos para descripción/monto y un `DropdownButtonFormField` para la categoría.

La UI intenta convertir el monto. Si no puede, conserva el diálogo y muestra `Escribe un monto válido.`. Si el texto sí representa un número, construye `Expense`; las reglas restantes siguen viviendo en el dominio.

No dupliques todas las validaciones en el widget. La UI traduce y presenta; el objeto protege su contrato.

## Pruebas de widget

Ver [`../app/test/widget_test.dart`](../app/test/widget_test.dart).

Una prueba de widget puede:

1. construir `PocketLedgerApp` con estado controlado;
2. tocar el botón;
3. escribir campos;
4. guardar;
5. observar gasto y total.

Otra prueba demuestra el failure mode de monto no numérico y comprueba que el diálogo sigue abierto.

[EJECUTAR]

```bash
flutter test test/widget_test.dart
flutter analyze
```

## Tu turno

Haz que el error por descripción vacía también sea visible en el diálogo y agrega una prueba de widget. No copies la regla de `trim().isEmpty` al widget; reutiliza el error que ya produce `Expense`.

## Checkpoint

Completa [Checkpoint 01 — resumen por categoría](../exercises/checkpoint-01.md).

## Errores comunes

- Probar sólo clases y nunca el comportamiento visible.
- Atrapar `catch (_) {}` y ocultar cualquier defecto como si fuera error del usuario.
- Usar `double` como representación persistente de dinero sólo porque el formulario recibe decimales.
- Introducir arquitectura compleja antes de que exista una necesidad visible.

## Siguiente paso

La UI ya crea objetos válidos. Ahora prepararemos esos objetos para persistencia sin acoplar el dominio al filesystem.

[Continúa con la lección 05](05-convierte-gastos-a-json.md).

## Referencias

- https://docs.flutter.dev/cookbook/forms/validation
- https://docs.flutter.dev/testing/overview
- https://api.flutter.dev/flutter/flutter_test/flutter_test-library.html
