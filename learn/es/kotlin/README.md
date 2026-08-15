# Curso de Kotlin desde cero — Construye una app Android offline de órdenes de trabajo

Kotlin es un lenguaje moderno y tipado usado ampliamente en Android y también en JVM, backend y multiplataforma. Este curso parte desde cero y hace crecer **FieldFlow**, una aplicación Android offline para registrar, priorizar y completar órdenes de trabajo en campo.

## Qué vas a construir

FieldFlow comienza con un núcleo Kotlin puro que modela órdenes y reglas de negocio sin depender de Android. Después añade persistencia local durable y sustituye controladamente esa frontera por Room antes de incorporar una interfaz Compose. Esta secuencia permite aprender Kotlin y diseño de fronteras antes de ocultarlos detrás de un framework.

## Estado

**Completo: 17/17 lecciones.** El repositorio contiene y valida tanto el núcleo Kotlin/JVM como un módulo Android compilable con Room y Compose.

## Tooling verificado

- Kotlin 2.4.10 para el núcleo JVM y Compose compiler.
- JDK 17.
- Gradle 9.6.1.
- Android Gradle Plugin 9.3.0 con **Built-in Kotlin**, como exige la ruta moderna de AGP 9.
- Android SDK 36 y Build Tools 36.0.0.
- Compose BOM 2026.06.00.
- Room 2.8.4 con KSP 2.3.10.
- Kotlinx Serialization 1.11.0 para el primer adaptador durable del núcleo JVM.

## Lecciones

1. [Tu primera orden de trabajo](lessons/01-tu-primera-orden.md)
2. [Modela datos con data class y enum](lessons/02-modela-el-dominio.md)
3. [Colecciones y reglas de prioridad](lessons/03-colecciones-y-reglas.md)
4. [Errores explícitos y pruebas](lessons/04-errores-y-pruebas.md)
5. [Sealed types para resultados explícitos](lessons/05-sealed-results.md)
6. [Casos de uso sin depender de Android](lessons/06-casos-de-uso.md)
7. [Una frontera para persistencia](lessons/07-frontera-persistencia.md)
8. [Integra el flujo y protege comportamiento](lessons/08-integra-y-protege.md)
9. [Serializa datos sin contaminar el dominio](lessons/09-serializa-datos.md)
10. [Persistencia durable detrás del repositorio](lessons/10-persistencia-durable.md)
11. [Escrituras seguras y fallos de almacenamiento](lessons/11-escrituras-seguras.md)
12. [Diseña el salto a Room sin romper el núcleo](lessons/12-prepara-room.md)
13. [Implementa Room como adaptador Android](lessons/13-room-adapter.md)
14. [Modela estado observable para la UI](lessons/14-estado-ui.md)
15. [Construye una pantalla Compose desde el estado](lessons/15-compose.md)
16. [Diseña FieldFlow offline first](lessons/16-offline-first.md)
17. [Evaluación final: entrega FieldFlow](lessons/17-evaluacion-final.md)

### Checkpoints

- [Checkpoint 02 — Fronteras y resultados](lessons/checkpoint-02.md)
- [Checkpoint 03 — Persistencia offline](lessons/checkpoint-03.md)
- [Checkpoint 04 — Android offline first](lessons/checkpoint-04.md)

## Instalar, build, test y run

Necesitas JDK 17+ y Gradle 9.6.1.

### Núcleo Kotlin/JVM

Desde `app/`:

```bash
gradle test
gradle run
```

### Evidencia Android

El subproyecto [`android/`](android/README.md) contiene una aplicación Android real con Room y Compose. Con Android SDK 36 instalado:

```bash
gradle :app:assembleDebug :app:testDebugUnitTest
```

GitHub Actions ejecuta ambos recorridos. El APK debug se genera bajo `android/app/build/outputs/apk/debug/`.

## Qué sabrás hacer al terminar

Leer y escribir Kotlin idiomático, modelar datos y nullability, usar colecciones, funciones y lambdas, manejar resultados y errores, probar comportamiento, estructurar una aplicación, persistir offline con Room y construir una UI Android Compose que consume estado observable.

## Contexto profesional

Kotlin es una opción central para desarrollo Android moderno. El curso no promete empleo: construye evidencia práctica transferible a mantenimiento y desarrollo de aplicaciones móviles.

## Referencias oficiales

- https://kotlinlang.org/docs/home.html
- https://kotlinlang.org/docs/releases.html
- https://kotlinlang.org/docs/serialization.html
- https://developer.android.com/kotlin
- https://developer.android.com/build/releases/agp-9-3-0-release-notes
- https://developer.android.com/build/migrate-to-built-in-kotlin
- https://developer.android.com/training/data-storage/room
- https://developer.android.com/topic/architecture/data-layer/offline-first
- https://developer.android.com/develop/ui/compose

## Siguiente paso

Empieza por [la lección 1](lessons/01-tu-primera-orden.md). Para control de versiones usa el [curso transversal de Git](../git/README.md) en lugar de duplicar un mini-curso aquí.
