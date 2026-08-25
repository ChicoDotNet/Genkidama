# Lección 07 — Estado de aplicación y asincronía sin esconder fallos

## Qué vas a conseguir

Moverás la coordinación entre dominio, persistencia y UI a `ExpenseController`, una frontera observable y testeable.

## El problema

Un widget puede llamar al store directamente, pero entonces cada pantalla necesita saber cuándo cargar, cuándo guardar, qué hacer si falla y cómo mantener consistente el total. Esa responsabilidad crecería con la aplicación.

## Código real

Abre [`../app/lib/application/expense_controller.dart`](../app/lib/application/expense_controller.dart).

El controlador mantiene tres cosas distintas:

1. el `ExpenseLedger` con reglas y cálculos;
2. el `ExpenseStore` que hace I/O;
3. un estado de carga: `idle`, `loading`, `ready` o `failed`.

`load()` es `async`: espera la lectura antes de reemplazar el ledger. Si la lectura falla, conserva un estado `failed` y un mensaje explícito.

`addExpense()` hace algo especialmente importante: construye primero el snapshot candidato, **lo persiste y sólo después lo publica en memoria**. Si escribir falla, la pantalla no muestra un gasto que desaparecería al reiniciar.

`ChangeNotifier` se usa aquí porque Flutter ya lo incluye y la necesidad es pequeña. No agregamos un paquete de gestión de estado sólo para enseñar una biblioteca adicional.

[DEMO]

Revisa [`../app/test/expense_controller_test.dart`](../app/test/expense_controller_test.dart). Las pruebas usan un store en memoria para provocar lecturas/escrituras válidas y fallidas de forma determinista.

## Tu turno

Agrega una prueba que escuche `notifyListeners()` y compruebe que una carga exitosa termina en `ready`. No afirmes un número exacto de notificaciones salvo que ese número sea realmente parte del contrato.

## Errores comunes

- Hacer I/O en getters o dentro de `build()`.
- Mutar el ledger antes de saber que `save()` terminó.
- Confundir “estado observable” con “estado global”.
- Instalar una arquitectura completa de estado cuando un objeto pequeño resuelve la necesidad actual.

## Siguiente paso

Conectaremos esta frontera a la pantalla y trataremos los errores de persistencia como comportamiento visible, no como mensajes perdidos en consola.

[Continúa con la lección 08](08-ui-persistente-y-fallos-visibles.md).

## Referencias

- https://api.flutter.dev/flutter/foundation/ChangeNotifier-class.html
- https://dart.dev/libraries/async/async-await
