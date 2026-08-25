# Lección 01 — Dart, Flutter y una app que ya corre

## Qué vas a conseguir

Ejecutarás las pruebas de PocketLedger y reconocerás las piezas mínimas de una app Flutter escrita en Dart.

## El problema

Empezar por veinte páginas de sintaxis no te enseña a construir. PocketLedger comienza con una pantalla real: total de gastos, lista vacía y botón para registrar uno nuevo.

## Antes de empezar

Instala Flutter 3.47 estable. Flutter ya incluye el Dart SDK, así que no necesitas mantener dos instalaciones separadas para este curso.

[DEMO]

```bash
flutter --version
dart --version
cd learn/es/dart/app
flutter pub get
flutter test
```

Después, si tienes un dispositivo, emulador o target Flutter disponible:

```bash
flutter run
```

## Concepto

Dart es el lenguaje. Flutter es el toolkit de UI y runtime que usa Dart para construir interfaces multiplataforma. `pubspec.yaml` declara versión, dependencias y metadata del paquete.

La función `main()` es el punto de entrada. `runApp` recibe el widget raíz. Un widget describe una parte de la interfaz; no es una pantalla dibujada manualmente píxel por píxel.

## Código real

- [`../app/pubspec.yaml`](../app/pubspec.yaml)
- [`../app/lib/main.dart`](../app/lib/main.dart)

Observa `PocketLedgerApp`, `ExpenseHomePage` y el `Scaffold`. Todavía no necesitas memorizar cada widget. Necesitas reconocer la dirección: entrada → árbol de widgets → estado visible.

## Tu turno

Cambia temporalmente el título `PocketLedger` por otro texto y ejecuta `flutter test`. ¿Qué prueba falla? Revierte el cambio después del experimento.

## Errores comunes

- Instalar Dart por separado y terminar con versiones incompatibles con Flutter.
- Confundir “la app compila” con “el comportamiento está probado”.
- Ejecutar `flutter run` sin tener un target disponible y asumir que el código está roto.

## Cómo comprobar

`flutter test` debe terminar en verde. La siguiente lección mueve la primera regla importante fuera de la UI.

## Siguiente paso

Continúa con [Lección 02 — Modela dinero sin perder centavos](02-modela-dinero-sin-perder-centavos.md).

## Referencias

- https://dart.dev/get-dart
- https://docs.flutter.dev/get-started/fundamentals
