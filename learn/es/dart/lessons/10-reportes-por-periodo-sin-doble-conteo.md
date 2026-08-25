# Lección 10 — Reportes por periodo sin doble conteo

## Qué vas a conseguir

PocketLedger calculará un resumen mensual con total, número de gastos y subtotales por categoría.

## El problema

Los reportes por fecha fallan fácilmente en los límites: un gasto a medianoche puede contarse dos veces si dos periodos consideran ambos extremos inclusivos.

## Concepto

Usaremos intervalos semiabiertos: `[inicio, fin)`. El inicio cuenta; el final pertenece al siguiente periodo.

## Código real

Revisa [`../app/lib/domain/expense_report.dart`](../app/lib/domain/expense_report.dart).

`ExpenseReport.between` valida el rango, recorre gastos una sola vez y acumula enteros en centavos. `ExpenseReport.forMonth` calcula el primer día del mes y el primer día del siguiente.

El resultado expone `totalsByCategory` como mapa no modificable. El reporte es una lectura derivada; no hace I/O ni modifica el ledger.

[DEMO]

Revisa [`../app/test/expense_report_test.dart`](../app/test/expense_report_test.dart). Una prueba coloca un gasto exactamente el 1 de septiembre y demuestra que no pertenece al reporte de agosto.

## Tu turno

Agrega una prueba para diciembre y comprueba que el final del periodo sea 1 de enero del siguiente año.

## Errores comunes

- Comparar sólo `month` e ignorar el año.
- Usar `double` para sumar dinero.
- Incluir `endExclusive` y duplicar movimientos en dos reportes.

## Siguiente paso

La lección 11 llevará el reporte a la UI sin crear una segunda fuente de estado.

## Referencias

- https://api.dart.dev/dart-core/DateTime-class.html
- https://dart.dev/language/collections

[Siguiente: Lección 11 — reporte visible](11-reporte-visible-con-la-misma-fuente-de-verdad.md)
