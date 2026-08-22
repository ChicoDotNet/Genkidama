# Lección 13 — Diagnostica sin filtrar datos personales

## Qué vas a conseguir
Añadirás una superficie de diagnóstico útil para soporte sin imprimir descripciones de gastos ni duplicar el estado de la aplicación.

## El problema
Cuando una app falla, `print(controller.expenses)` parece cómodo, pero mezcla observabilidad con datos introducidos por la persona usuaria. Un diagnóstico profesional debe responder preguntas operativas con la mínima información necesaria.

## Concepto
PocketLedger ahora expone `ExpenseDiagnostics`: estado de carga, cantidad de gastos, total en centavos y conteos por categoría. Es un snapshot derivado; no es otra fuente de verdad.

[DEMO] Abre `app/lib/application/expense_controller.dart` y localiza `diagnostics()`.

```text
estado vivo -> diagnostics() -> snapshot inmutable -> soporte/pruebas
```

La regla es deliberada: **ninguna descripción del gasto cruza esta frontera**.

## Código real
Ver implementación: [`../app/lib/application/expense_controller.dart`](../app/lib/application/expense_controller.dart).

Observa tres decisiones:

1. `categoryCounts` se expone como vista no modificable.
2. El total conserva centavos enteros; diagnóstico no reinterpreta dinero.
3. El snapshot se calcula desde `_ledger`, no se mantiene sincronizado manualmente.

## Errores comunes
- Registrar objetos de dominio completos “por si acaso”.
- Añadir un segundo contador mutable que puede divergir.
- Ocultar errores de carga y reportar `ready` aunque el store haya fallado.

## Tu turno
Añade una prueba que cargue gastos con descripciones sensibles y compruebe que el diagnóstico sólo contiene agregados.

[EJECUTAR]

```bash
cd learn/es/dart/app
flutter test test/expense_controller_test.dart
```

Ver solución después de intentarlo: [`../solutions/checkpoint-04-diagnostico.md`](../solutions/checkpoint-04-diagnostico.md).

## Resumen
Observabilidad no significa recolectar todo. Un junior confiable aprende a diagnosticar con señales suficientes y datos mínimos.

## Siguiente paso
[Lección 14 — Debugging con evidencia](14-debugging-con-evidencia.md)

## Referencias
- [Dart diagnostics](https://dart.dev/tools)
- [Flutter DevTools](https://docs.flutter.dev/tools/devtools)
