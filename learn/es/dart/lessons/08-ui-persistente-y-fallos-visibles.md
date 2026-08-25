# Lección 08 — UI persistente y fallos que el usuario puede entender

## Qué vas a conseguir

PocketLedger cargará su archivo al arrancar, guardará antes de mostrar un gasto nuevo y bloqueará escrituras cuando detecte datos que no puede leer con seguridad.

## El problema

Persistir no es sólo “guardar”. También hay que decidir qué ve la persona mientras el estado se carga y qué ocurre si el archivo existe pero no puede interpretarse.

## Código real

Revisa [`../app/lib/main.dart`](../app/lib/main.dart).

`main()` ahora inicializa Flutter, obtiene el directorio de documentos, crea `JsonFileExpenseStore`, carga `ExpenseController` y sólo entonces construye la aplicación.

`ExpenseHomePage` observa el controlador. Ya no posee un `ExpenseLedger` privado: la pantalla presenta estado, recoge entrada y solicita una operación.

Si la carga inicial falla, aparece un diagnóstico y desaparece el botón de alta. Esa decisión evita que un nuevo `save()` sobrescriba un archivo que quizá contiene información recuperable.

Si falla sólo una escritura, el diálogo permanece abierto y muestra el error. Como `ExpenseController` publica después de persistir, la lista tampoco cambia.

[DEMO]

Revisa [`../app/test/widget_test.dart`](../app/test/widget_test.dart). Los tests no dependen del filesystem ni de un teléfono: inyectan un `MemoryExpenseStore` y prueban la conducta visible de éxito y error.

[EJECUTAR]

```bash
dart format --output=none --set-exit-if-changed lib test
flutter analyze
flutter test
```

## Tu turno

Ejecuta PocketLedger, registra dos gastos, cierra y abre la aplicación y confirma que regresan. Después lee el JSON generado únicamente para inspeccionarlo; no lo edites mientras la app escribe.

## Checkpoint

Completa [Checkpoint 02 — elimina un gasto sin perder consistencia](../exercises/checkpoint-02.md).

## Errores comunes

- Hacer `save()` sin `await` y cerrar el diálogo inmediatamente.
- Mostrar un cambio en memoria aunque la persistencia falló.
- Borrar automáticamente un archivo corrupto para “recuperarse”.
- Probar la UI contra rutas reales del equipo cuando una frontera inyectable permite pruebas deterministas.

## Siguiente paso

Ahora que una sola frontera gobierna estado y persistencia, podemos derivar filtros y reportes sin crear otra fuente de verdad.

[Siguiente: Lección 09 — consulta gastos sin mutar estado](09-consulta-gastos-sin-mutar-estado.md)

## Referencias

- https://docs.flutter.dev/data-and-backend/state-mgmt/simple
- https://docs.flutter.dev/cookbook/persistence
- https://docs.flutter.dev/testing/overview
