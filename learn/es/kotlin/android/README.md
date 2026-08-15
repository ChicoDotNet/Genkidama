# FieldFlow Android proof

Este subproyecto demuestra de forma ejecutable la frontera Android enseñada en las lecciones 13–16 sin convertir el núcleo Kotlin/JVM en un proyecto dependiente del framework.

## Qué demuestra

- una aplicación Android real compilable con AGP;
- persistencia Room con `@Entity`, `@Dao` y `@Database`;
- observación mediante `Flow`;
- UI Jetpack Compose que refleja el estado de Room;
- escritura y actualización desde coroutines;
- JDK 17 y API Android actuales compatibles con el curso.

El núcleo de reglas y sus pruebas permanecen en `../app/`. Este módulo es evidencia de integración de plataforma, no una segunda fuente de reglas de negocio.

## Build

Desde esta carpeta:

```bash
gradle :app:assembleDebug :app:testDebugUnitTest
```

El workflow `Learn Kotlin` instala la plataforma Android necesaria y ejecuta este comando en Linux. El APK debug resultante se genera bajo `app/build/outputs/apk/debug/`.

## Versiones verificadas por CI

- Android Gradle Plugin 9.3.0.
- Kotlin 2.4.10.
- Compose BOM 2026.06.00.
- Room 2.8.4.
- Gradle 9.6.1.
- JDK 17.

Room 3.0.1 ya existe, pero esta prueba mantiene Room 2.8.4 porque el objetivo del curso es una integración Android estable y pequeña; la migración a Room 3 implica además adoptar su nuevo contrato KSP/coroutines y no es necesaria para demostrar las competencias junior de esta aplicación.
