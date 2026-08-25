# Lección 14 — Debugging con evidencia

## Qué vas a conseguir
Aprenderás a aislar un fallo de PocketLedger usando estado observable, pruebas focalizadas y herramientas del ecosistema antes de cambiar código.

## El problema
Cambiar varias cosas “a ver si se arregla” destruye la evidencia. En una app con UI, estado y persistencia necesitas separar tres preguntas: ¿falló la lectura?, ¿falló la regla?, ¿falló la representación visible?

## Concepto
Usa una secuencia corta y repetible:

1. reproduce;
2. captura el estado mínimo con `diagnostics()`;
3. reduce el caso a una prueba;
4. corrige una causa;
5. ejecuta formatter, analyzer y tests.

[EN PANTALLA]

```bash
flutter test test/expense_controller_test.dart
flutter analyze
dart format --output=none --set-exit-if-changed lib test
```

Flutter DevTools complementa esta secuencia para inspeccionar árbol de widgets, rendimiento y memoria. No sustituye una prueba de regresión cuando el defecto vive en una regla determinista.

## Código real
La frontera que permite observar sin filtrar datos vive en [`../app/lib/application/expense_controller.dart`](../app/lib/application/expense_controller.dart).

## Errores comunes
- Atrapar todas las excepciones y convertirlas en “algo salió mal”.
- Registrar objetos completos del usuario.
- modificar producción antes de conseguir una reproducción estable;
- confundir un warning del analyzer con la causa sin comprobarlo.

## Tu turno
Provoca en un test un `ExpenseStoreException` de lectura. Comprueba `ExpenseLoadState.failed`, el mensaje contextual y un snapshot de diagnóstico que no inventa gastos.

## Cómo comprobar tu solución
La suite debe seguir verde y el diagnóstico no debe contener ninguna descripción fixture.

## Resumen
Debugging profesional reduce incertidumbre. Cada paso debe dejar más evidencia que el anterior.

## Siguiente paso
[Lección 15 — Verifica una entrega portable](15-verifica-una-entrega-portable.md)

## Referencias
- [Flutter DevTools](https://docs.flutter.dev/tools/devtools)
- [Dart analyzer](https://dart.dev/tools/dart-analyze)
