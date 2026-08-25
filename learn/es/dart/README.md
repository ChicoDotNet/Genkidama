# Dart desde cero — PocketLedger

Aprenderás Dart construyendo **PocketLedger**, un gestor de gastos personales con Flutter. El curso usa Dart 3.13 y Flutter 3.47. La aplicación combina dominio Dart, UI Flutter real, persistencia JSON versionada, estado asíncrono explícito, reportes temporales, recuperación ante fallos y diagnósticos operativos que evitan filtrar texto introducido por la persona usuaria.

## Qué necesitas

- Flutter 3.47.0 estable, que incluye Dart 3.13.
- VS Code o un editor equivalente.
- Para ejecutar la UI: un dispositivo/emulador o una plataforma Flutter disponible.

Git se aprende en el [curso transversal de Git](../git/); aquí sólo lo enlazamos cuando hace falta control de versiones.

## Ejecutar y probar

Desde `learn/es/dart/app`:

```bash
flutter pub get
dart format lib test
flutter analyze
flutter test
flutter build web --release
flutter run
```

CI ejecuta formatter, analyzer y tests en Ubuntu y Windows, y construye el artefacto web en Ubuntu. El build web demuestra que PocketLedger puede empaquetarse para ese target; no equivale a certificación para App Store, Google Play ni dispositivos móviles concretos.

## Lecciones

1. [Dart, Flutter y una app que ya corre](lessons/01-dart-flutter-y-una-app-que-corre.md)
2. [Modela dinero sin perder centavos](lessons/02-modela-dinero-sin-perder-centavos.md)
3. [Colecciones, estado y total de gastos](lessons/03-colecciones-estado-y-total.md)
4. [Formulario, errores y pruebas de widget](lessons/04-formulario-errores-y-pruebas.md)
5. [Convierte gastos a JSON sin romper el dominio](lessons/05-convierte-gastos-a-json.md)
6. [Persistencia local versionada y errores explícitos](lessons/06-persistencia-local-versionada.md)
7. [Estado de aplicación y asincronía sin esconder fallos](lessons/07-estado-de-aplicacion-y-asincronia.md)
8. [UI persistente y fallos que el usuario puede entender](lessons/08-ui-persistente-y-fallos-visibles.md)
9. [Consulta gastos sin mutar el estado](lessons/09-consulta-gastos-sin-mutar-estado.md)
10. [Reportes por periodo sin doble conteo](lessons/10-reportes-por-periodo-sin-doble-conteo.md)
11. [Reporte visible con la misma fuente de verdad](lessons/11-reporte-visible-con-la-misma-fuente-de-verdad.md)
12. [Recuperación explícita tras un fallo](lessons/12-recuperacion-explicita-tras-un-fallo.md)
13. [Diagnostica sin filtrar datos personales](lessons/13-diagnostica-sin-filtrar-datos.md)
14. [Debugging con evidencia](lessons/14-debugging-con-evidencia.md)
15. [Verifica una entrega portable](lessons/15-verifica-una-entrega-portable.md)
16. [Hardening antes de entregar](lessons/16-hardening-antes-de-entregar.md)
17. [Evaluación final: entrega PocketLedger](lessons/17-evaluacion-final.md)

### Checkpoints y evaluación

- [Checkpoint 01 — resumen por categoría](exercises/checkpoint-01.md)
- [Checkpoint 02 — elimina un gasto sin perder consistencia](exercises/checkpoint-02.md)
- [Checkpoint 03 — reporte mensual confiable](exercises/checkpoint-03.md)
- [Checkpoint 04 — diagnostica y endurece PocketLedger](exercises/checkpoint-04.md)
- [Evaluación final autónoma](exercises/final-pocketledger.md)

Las soluciones viven separadas y se consultan después del intento. La evaluación final tiene [solución de referencia](solutions/final-pocketledger.md), no una receta obligatoria.

## Qué sabrás hacer al terminar

- modelar reglas Dart con null safety y datos inmutables;
- representar dinero sin errores binarios de redondeo;
- separar dominio, estado, persistencia y UI;
- trabajar con `Future`, errores explícitos y estado observable;
- persistir y versionar JSON sin borrar datos corruptos silenciosamente;
- construir filtros y reportes deterministas;
- escribir pruebas de dominio, persistencia, controlador y widgets;
- depurar a partir de evidencia y convertir regresiones en pruebas;
- producir diagnósticos agregados sin filtrar PII;
- ejecutar format, análisis estático, tests y un build de entrega;
- explicar decisiones y trade-offs con evidencia del proyecto.

## Arquitectura de PocketLedger

La dependencia principal es deliberadamente pequeña:

```text
Flutter UI -> ExpenseController -> dominio
                          \-> ExpenseStore -> archivo JSON
```

El dominio no conoce widgets ni archivos. El controlador publica un cambio sólo después de persistirlo correctamente. Los reportes son vistas derivadas del mismo ledger, no una segunda fuente de verdad.

## Preguntas frecuentes

### ¿Necesito saber programar antes?
No. El curso empieza desde la ejecución y define conceptos cuando desbloquean una capacidad visible.

### ¿Dart y Flutter son lo mismo?
No. Dart es el lenguaje; Flutter es el toolkit de UI que usa Dart. PocketLedger enseña ambos sin ocultar las reglas Dart detrás del framework.

### ¿Por qué guardamos dinero en centavos?
Porque un entero representa exactamente la unidad mínima elegida y evita usar aproximaciones binarias para una regla monetaria básica.

### ¿Por qué no se borra un archivo corrupto automáticamente?
Porque convertir un fallo de lectura en “no hay gastos” escondería una posible pérdida de datos. PocketLedger muestra el error y permite reintentar.

### ¿El build web significa que la app está publicada en móviles?
No. Sólo es evidencia reproducible de empaquetado para web. Android/iOS, firma, tiendas y revisión de UX tienen requisitos adicionales.

### ¿Necesito 100% de code coverage?
No. El repositorio prioriza contratos, failure modes y regresiones útiles. Cuando coverage sea medible, 44% es un piso suficiente bajo esa condición; más es bienvenido, no obligatorio.

## Glosario

- **null safety:** reglas del lenguaje que distinguen valores anulables y no anulables.
- **widget:** unidad declarativa de interfaz en Flutter.
- **estado:** datos que determinan qué debe mostrar o permitir la aplicación en un momento dado.
- **persistencia:** almacenamiento que sobrevive al proceso actual.
- **serialización:** transformación de objetos a una representación almacenable, aquí JSON.
- **ledger:** colección ordenada de gastos que funciona como fuente de verdad del dominio.
- **intervalo semiabierto:** rango que incluye el inicio y excluye el final, útil para evitar doble conteo entre periodos consecutivos.
- **regresión:** comportamiento que funcionaba y vuelve a romperse después de un cambio.
- **diagnóstico:** información operacional para entender estado/fallos sin exponer contenido sensible innecesario.

## Cómo hablar de este proyecto en una entrevista

Explica primero el problema y después señala código/pruebas concretas. Una conversación razonable incluye:

- por qué elegiste centavos enteros;
- cómo separaste reglas, UI y persistencia;
- cómo `persist-before-publish` evita inconsistencias;
- por qué un archivo corrupto produce un error visible;
- cómo evitas doble conteo en reportes mensuales;
- qué información excluyes de diagnósticos;
- qué diferencia existe entre formatter, analyzer, tests y build;
- un defecto real que CI expuso y cómo lo convertiste en una frontera verificable;
- qué cambiarías si la aplicación necesitara otra forma de persistencia.

No presentes PocketLedger como experiencia profesional que no tienes. Preséntalo como evidencia concreta de lo que puedes construir, probar y explicar.

## Referencias oficiales

- [Dart documentation](https://dart.dev/)
- [Dart language](https://dart.dev/language)
- [Effective Dart](https://dart.dev/effective-dart)
- [Flutter documentation](https://docs.flutter.dev/)
- [Flutter testing](https://docs.flutter.dev/testing/overview)
- [Flutter deployment](https://docs.flutter.dev/deployment)
- [path_provider](https://pub.dev/packages/path_provider)

## Empleabilidad

Dart se utiliza profesionalmente sobre todo junto con Flutter para aplicaciones multiplataforma. Este curso busca una base Junior razonable; no promete empleo ni sustituye conocimientos de UX, APIs, backend, accesibilidad, seguridad móvil o publicación en tiendas.

## Siguiente paso

Después de completar la evaluación final, conserva PocketLedger para practicar mantenimiento y cambios pequeños. En la secuencia v1 de Genkidama Learn, el siguiente lenguaje es **C**, una vez que Dart esté integrado y estable.
