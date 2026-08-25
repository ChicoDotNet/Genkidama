# Lección 17 — Evaluación final: entrega PocketLedger

## Qué vas a conseguir

Demostrarás que puedes leer, modificar, probar y explicar una aplicación Dart/Flutter existente sin seguir una receta paso a paso.

## Antes de empezar

Completa las lecciones 1–16 y los cuatro checkpoints. Esta evaluación combina dominio Dart, null safety, colecciones inmutables, asincronía, persistencia JSON, estado de aplicación, Flutter, reportes, diagnóstico responsable, debugging y entrega.

## El problema

Una base Junior útil no consiste en recordar widgets o copiar snippets. Necesitas entrar a código existente, localizar responsabilidades, corregir un defecto, añadir una capacidad, protegerla con pruebas y explicar los trade-offs.

## Encargo final

Trabaja primero con el [ejercicio final versionado](../exercises/final-pocketledger.md). No abras la solución hasta haber intentado las seis historias.

### Historia 1 — Presupuesto mensual por categoría

Añade una capacidad pequeña que permita comparar el gasto mensual de una categoría contra un presupuesto expresado en centavos. Mantén la regla fuera de los widgets y evita `double` para dinero.

### Historia 2 — Corrige un bug de fecha

Supón que un gasto persistido con fecha UTC cae en el día anterior/siguiente al observarlo en hora local y un reporte mensual lo clasifica incorrectamente. Decide qué semántica de fecha debe tener PocketLedger, corrige una sola frontera y añade una prueba de regresión que cruce un límite de mes.

### Historia 3 — Error de persistencia con contexto seguro

Mejora un error de lectura o escritura para que ayude a diagnosticar qué operación falló sin registrar descripciones de gastos ni reemplazar silenciosamente un archivo corrupto por un ledger vacío.

### Historia 4 — Comportamiento visible

Muestra en Flutter la señal de presupuesto de la Historia 1 y añade una prueba de widget que demuestre tanto el caso dentro del presupuesto como el caso excedido.

### Historia 5 — Diagnóstico operativo

Extiende `ExpenseDiagnostics` con una señal agregada útil para la nueva capacidad, sin exponer descripciones ni otro texto introducido por la persona usuaria.

### Historia 6 — Evolución de diseño

PocketLedger podría sincronizar datos en el futuro. Diseña la frontera mínima que permitiría una segunda implementación de persistencia sin implementar nube, autenticación ni red. Explica qué mantendrías fuera de alcance y por qué.

## Evidencia obligatoria

Tu entrega debe incluir:

1. código Dart modificado con responsabilidades claras;
2. una prueba de regresión para el bug de fecha;
3. una prueba de widget para la nueva capacidad visible;
4. manejo explícito de error sin pérdida silenciosa ni PII en diagnóstico;
5. `dart format`, `flutter analyze`, `flutter test` y un build aplicable verdes;
6. una referencia concreta a documentación oficial consultada;
7. una nota breve sobre la mejora de diseño que deliberadamente no implementaste.

## Cómo comprobar

Desde `learn/es/dart/app`:

```bash
flutter pub get
dart format lib test
flutter analyze
flutter test
flutter build web --release
```

La política del repositorio no exige 100% de coverage. Si incorporas una medición razonable, 44% es piso suficiente cuando contratos, failure modes y regresiones relevantes están protegidos; una cifra mayor es bienvenida, no una obligación.

## Rúbrica — 100 puntos

| Área | Puntos | Evidencia esperada |
|---|---:|---|
| Dart idiomático y dominio | 20 | null safety, dinero entero, objetos claros y reglas fuera de UI |
| Estado, asincronía y persistencia | 15 | consistencia, errores explícitos y frontera de almacenamiento defendible |
| Flutter y comportamiento visible | 15 | UI coherente con la misma fuente de verdad y prueba de widget útil |
| Errores, privacidad y datos | 15 | sin borrado silencioso, sin PII en diagnóstico y fechas con semántica explícita |
| Pruebas y regresión | 20 | bugfix y nueva capacidad protegidos, no sólo happy paths triviales |
| Calidad y entrega | 5 | format/analyze/test/build reproducibles |
| Explicación profesional | 10 | documentación consultada, trade-offs y alcance futuro defendibles |

### Interpretación

- **85–100:** evidencia sólida para intentar trabajo Junior/Entry Level con supervisión dentro de este alcance.
- **70–84:** base razonable; refuerza las áreas de menor puntuación antes de presentar el proyecto.
- **<70:** vuelve al checkpoint relacionado y repite la historia que expuso la brecha.

La rúbrica evalúa tu trabajo sobre PocketLedger; no promete empleo.

## Cómo hablar de este proyecto en una entrevista

Prepárate para responder señalando código y pruebas reales:

1. ¿Por qué el dinero se representa en centavos enteros?
2. ¿Qué garantiza `persist-before-publish` cuando una escritura falla?
3. ¿Por qué los reportes usan un intervalo semiabierto `[inicio, fin)`?
4. ¿Qué información excluyes deliberadamente de `ExpenseDiagnostics`?
5. ¿Qué diferencia hay entre `flutter analyze`, `flutter test` y `flutter build`?
6. ¿Qué aprendiste al validar el proyecto en Ubuntu y Windows?
7. ¿Qué demuestra un build web y qué no demuestra sobre Android/iOS o tiendas?
8. ¿Qué frontera cambiarías primero para soportar otra persistencia?
9. ¿Qué bug real te obligó a convertir un supuesto en una prueba?

No memorices frases. Explica problema → decisión → evidencia → trade-off.

## Solución de referencia

Sólo después de intentar el ejercicio, compara tus decisiones con la [solución de referencia](../solutions/final-pocketledger.md). No necesitas coincidir exactamente si conservas contratos, pruebas el comportamiento y puedes explicar tus decisiones.

## Referencias

- https://dart.dev/language
- https://dart.dev/effective-dart
- https://api.dart.dev/dart-core/DateTime-class.html
- https://docs.flutter.dev/testing/overview
- https://docs.flutter.dev/deployment/web

## Siguiente paso

Conserva PocketLedger como proyecto de práctica y sigue haciendo cambios pequeños sobre código existente. Para ramas, historia y colaboración utiliza el [curso transversal de Git](../../git/README.md).
