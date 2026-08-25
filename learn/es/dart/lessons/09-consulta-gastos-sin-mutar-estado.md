# Lección 09 — Consulta gastos sin mutar el estado

## Qué vas a conseguir

PocketLedger podrá mostrar todos los gastos o sólo una categoría sin duplicar listas mutables ni cambiar el estado persistido.

## El problema

Una pantalla necesita filtros, pero filtrar no significa modificar la colección original. Si cada widget crea su propia copia mutable, aparecen fuentes de verdad paralelas.

## Concepto

Dart permite transformar colecciones con `where`, `map` y otros métodos que devuelven iterables. La aplicación puede derivar una vista y mantener el dominio intacto.

## Código real

Revisa [`../app/lib/application/expense_controller.dart`](../app/lib/application/expense_controller.dart).

`expensesForCategory` devuelve una lista no modificable. Cuando la categoría es `null`, representa “todas”. Cuando existe un filtro, usa `where` sobre el ledger y vuelve a cerrar la salida con `List.unmodifiable`.

[DEMO]

En [`../app/lib/main.dart`](../app/lib/main.dart) el selector de categoría sólo guarda el criterio de presentación. No escribe al JSON ni reemplaza el ledger.

## Tu turno

Agrega dos gastos de categorías distintas y cambia el filtro varias veces. Confirma que el total global no cambia y que al volver a “Todas” reaparecen ambos.

## Cómo comprobar tu solución

```bash
flutter analyze
flutter test
```

## Errores comunes

- Borrar elementos del ledger para “filtrar”.
- Exponer directamente una lista mutable.
- Guardar el filtro como si fuera parte de cada gasto.

## Siguiente paso

En la lección 10 construiremos un reporte temporal sin mezclar cálculos con widgets.

## Referencias

- https://dart.dev/libraries/dart-core#collections
- https://api.dart.dev/dart-core/Iterable/where.html

[Siguiente: Lección 10 — reportes por periodo](10-reportes-por-periodo-sin-doble-conteo.md)
