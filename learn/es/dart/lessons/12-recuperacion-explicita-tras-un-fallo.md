# Lección 12 — Recuperación explícita tras un fallo

## Qué vas a conseguir

PocketLedger permitirá reintentar una carga fallida sin borrar datos ni volver a crear la aplicación.

## El problema

Un error de lectura puede ser temporal: permisos, almacenamiento no montado o un archivo que otra operación todavía está escribiendo. “Recuperarse” borrando el archivo sería una pérdida de datos disfrazada de éxito.

## Código real

`ExpenseController.retryLoad()` delega en la misma operación `load()`. Eso conserva una sola semántica: pasa a `loading`, limpia el mensaje anterior y sólo reemplaza el ledger cuando la lectura termina correctamente.

En [`../app/lib/main.dart`](../app/lib/main.dart) un fallo de persistencia muestra el diagnóstico y el botón **Reintentar**. El botón de alta permanece deshabilitado mientras el estado siga fallido.

Esta decisión es deliberada: el usuario puede intentar de nuevo, inspeccionar el archivo o buscar ayuda sin que PocketLedger destruya evidencia.

## Tu turno

Modifica el `MemoryExpenseStore` de pruebas para que falle una sola vez y luego responda correctamente. Escribe un widget test que pulse `Reintentar` y compruebe que el botón de alta vuelve a estar disponible.

## Checkpoint

Completa [Checkpoint 03 — reporte mensual confiable](../exercises/checkpoint-03.md).

## Errores comunes

- Borrar el archivo ante cualquier excepción.
- Reintentar automáticamente en un ciclo infinito.
- Cambiar el ledger antes de que la lectura sea válida.
- Ocultar la causa con un mensaje genérico sin contexto.

## Siguiente paso

[Lección 13 — Diagnostica sin filtrar datos personales](13-diagnostica-sin-filtrar-datos.md)

## Referencias

- https://dart.dev/language/error-handling
- https://docs.flutter.dev/cookbook/persistence
- https://docs.flutter.dev/testing/overview
