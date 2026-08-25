# Lección 06 — Persistencia local versionada y errores explícitos

## Qué vas a conseguir

Guardarás PocketLedger en un archivo JSON del directorio de documentos de la aplicación y podrás recuperarlo en el siguiente arranque.

## El problema

Escribir `jsonEncode(gastos)` directamente desde un widget funciona una vez, pero mezcla UI, rutas del sistema, serialización y manejo de errores. Además, si mañana cambia el formato no sabrás qué versión estás leyendo.

## Código real

Revisa [`../app/lib/persistence/expense_store.dart`](../app/lib/persistence/expense_store.dart).

`ExpenseStore` define dos operaciones asincrónicas: `load()` y `save()`. `JsonFileExpenseStore` implementa esa frontera con `dart:io` y un documento con `schemaVersion`.

Un archivo inexistente significa una instalación nueva y devuelve una lista vacía. Un archivo existente pero corrupto es diferente: produce `ExpenseStoreException`. PocketLedger no debe borrar silenciosamente información sólo porque no pudo interpretarla.

La escritura usa un archivo temporal antes de reemplazar el archivo anterior. Es una mejora frente a escribir directamente sobre el único archivo, aunque más adelante todavía podremos endurecer la recuperación ante fallos del sistema.

En [`../app/lib/main.dart`](../app/lib/main.dart), `path_provider` obtiene un directorio apropiado para la aplicación. Es la única dependencia nueva de este incremento y evita inventar rutas específicas de Android, iOS, Linux o Windows. La versión usada está fijada en `pubspec.yaml`.

[EJECUTAR]

```bash
flutter pub get
flutter test test/expense_store_test.dart
```

## Tu turno

Crea un archivo temporal con una versión de esquema distinta de `1` y demuestra con una prueba que el store la rechaza. Después prueba un archivo que ni siquiera sea JSON válido.

## Errores comunes

- Interpretar “archivo no existe” y “archivo corrupto” como el mismo caso.
- Guardar en una ruta absoluta propia de tu computadora.
- Sobrescribir datos válidos después de una lectura fallida.
- Agregar una base de datos antes de necesitar consultas que justifiquen ese peso.

## Siguiente paso

La persistencia ya existe, pero un widget no debería coordinar lectura, escritura y reglas de negocio. Separaremos ese estado de aplicación.

[Continúa con la lección 07](07-estado-de-aplicacion-y-asincronia.md).

## Referencias

- https://api.dart.dev/dart-io/File-class.html
- https://pub.dev/packages/path_provider
- https://dart.dev/libraries/async/async-await
