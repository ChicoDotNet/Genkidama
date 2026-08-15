# Curso de Kotlin desde cero — Construye una app Android offline de órdenes de trabajo

Kotlin es un lenguaje moderno y tipado usado ampliamente en Android y también en JVM, backend y multiplataforma. Este curso parte desde cero y hace crecer **FieldFlow**, una aplicación Android offline para registrar, priorizar y completar órdenes de trabajo en campo.

## Qué vas a construir

FieldFlow comienza con un núcleo Kotlin puro que modela órdenes y reglas de negocio sin depender de Android. Después añadiremos persistencia local, interfaz Android y sincronización deliberada. Esta separación permite aprender Kotlin antes de ocultarlo detrás de un framework.

## Estado

Curso en construcción: **4/17 lecciones**.

## Tooling verificado

- Kotlin 2.4.10.
- JDK 17 como baseline compatible con Android Gradle Plugin actual.
- Gradle 9.5 cuando se incorpore el módulo Android.
- Primer slice: Kotlin/JVM + Gradle para que las reglas de negocio tengan feedback rápido y CI ligero.

## Lecciones

1. [Tu primera orden de trabajo](lessons/01-tu-primera-orden.md)
2. [Modela datos con data class y enum](lessons/02-modela-el-dominio.md)
3. [Colecciones y reglas de prioridad](lessons/03-colecciones-y-reglas.md)
4. [Errores explícitos y pruebas](lessons/04-errores-y-pruebas.md)

## Instalar, build, test y run

Necesitas JDK 17+. En el slice inicial:

```bash
gradle test
gradle run
```

Android Studio será necesario cuando el curso incorpore la aplicación Android; no es requisito para las primeras lecciones de lenguaje.

## Qué sabrás hacer al terminar

Leer y escribir Kotlin idiomático, modelar datos y nullability, usar colecciones, funciones y lambdas, manejar errores, probar comportamiento, estructurar una aplicación, persistir offline y construir una UI Android que consume el mismo dominio.

## Contexto profesional

Kotlin es la opción recomendada por Google para desarrollo Android moderno. El curso no promete empleo: construye evidencia práctica transferible a mantenimiento y desarrollo de aplicaciones móviles.

## Referencias oficiales

- https://kotlinlang.org/docs/home.html
- https://kotlinlang.org/docs/releases.html
- https://developer.android.com/kotlin
- https://developer.android.com/build/releases/agp-9-3-0-release-notes

## Siguiente paso

Empieza por [la lección 1](lessons/01-tu-primera-orden.md). Para control de versiones usa el [curso transversal de Git](../git/README.md) en lugar de duplicar un mini-curso aquí.
