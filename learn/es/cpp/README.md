# Curso de C++ desde cero — Construye un indexador y buscador de archivos

C++ es un lenguaje compilado de propósito general usado cuando importan rendimiento, integración nativa y control explícito de recursos. El curso construye una sola aplicación real: **ThreadSeek**, un indexador/buscador que evoluciona desde una baseline secuencial correcta hasta concurrencia medible y operación portable.

## Qué vas a construir

ThreadSeek recorre directorios, representa archivos con tipos claros, busca por nombre, persiste/reconstruye su índice, compara descubrimiento secuencial y paralelo, permite cancelación cooperativa, reporta progreso y tolera cambios normales del filesystem.

## Qué necesitas

- Compilador moderno con C++23. Baseline local probada: GCC 14.2.0.
- CMake 3.28 o posterior. Baseline local probada: CMake 3.31.6.
- VS Code es opcional.

Al verificar el curso, GCC publicaba 16.1 como línea estable principal y CMake 4.4.2 como release estable. Se documenta aparte la baseline realmente ejecutada.

## Build, test y run

```bash
cd learn/es/cpp/app
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build --parallel
ctest --test-dir build --output-on-failure
./build/threadseek . txt
```

## Avance

**17/17 lecciones implementadas.** La evaluación final ya existe; el curso permanece `in_progress` hasta que los gates del cierre y la metadata final queden verdes.

1. [Compila y ejecuta ThreadSeek](lessons/01-compila-y-ejecuta-threadseek.md)
2. [Modela archivos con tipos y `std::filesystem`](lessons/02-modela-archivos-con-tipos.md)
3. [Busca sobre el índice](lessons/03-busca-sobre-el-indice.md)
4. [Haz explícitos los errores y prueba comportamiento](lessons/04-errores-y-pruebas.md)
5. [Separa descubrimiento del índice](lessons/05-separa-descubrimiento-del-indice.md)
6. [RAII en recursos reales](lessons/06-raii-en-recursos-reales.md)
7. [Persiste y reconstruye el índice](lessons/07-persiste-y-reconstruye-el-indice.md)
8. [Diseña fallos de persistencia explícitos](lessons/08-fallos-de-persistencia.md)
9. [Mide la línea base antes de paralelizar](lessons/09-mide-la-linea-base.md)
10. [Particiona trabajo con `std::jthread`](lessons/10-particiona-trabajo-con-jthread.md)
11. [Minimiza estado mutable compartido](lessons/11-minimiza-estado-compartido.md)
12. [Compara sin perder determinismo](lessons/12-compara-sin-perder-determinismo.md)
13. [Cancela sin abandonar recursos](lessons/13-cancela-sin-abandonar-recursos.md)
14. [Tolera un filesystem que cambia](lessons/14-tolera-un-filesystem-que-cambia.md)
15. [Perfila antes de optimizar](lessons/15-perfila-antes-de-optimizar.md)
16. [Endurece portabilidad y entrega](lessons/16-endurece-portabilidad-y-entrega.md)
17. [Evaluación final: entrega ThreadSeek](lessons/17-evaluacion-final.md)

[Checkpoint 01 — Índice confiable](exercises/checkpoint-01.md) · [Solución](solutions/checkpoint-01.md)

[Checkpoint 02 — Índice durable](exercises/checkpoint-02.md) · [Solución](solutions/checkpoint-02.md)

[Checkpoint 03 — Concurrencia medible y determinista](exercises/checkpoint-03.md) · [Solución](solutions/checkpoint-03.md)

[Checkpoint 04 — Operación robusta](exercises/checkpoint-04.md) · [Solución](solutions/checkpoint-04.md)

[Ejercicio final](exercises/final-threadseek.md) · [Solución de referencia](solutions/final-threadseek.md)

## Qué sabrás hacer al terminar

Leer C++ moderno, comprender value semantics y ownership, usar RAII y STL, separar I/O de lógica, manejar errores, construir con CMake, probar comportamiento, medir rendimiento, cancelar trabajo cooperativamente y trabajar con concurrencia sin carreras obvias.

## Empleabilidad

Estas habilidades aparecen en software de sistemas, motores, tooling, multimedia, bibliotecas nativas e infraestructura. El curso no promete empleo: una vacante junior puede exigir además algoritmos, debugging y fundamentos de plataforma.

## Preguntas frecuentes

**¿Necesito saber C antes?** No. C++ se enseña como lenguaje propio.

**¿Por qué no empezamos con punteros manuales?** Porque ownership manual no es un rito de iniciación. Primero usamos valores, RAII y contenedores estándar.

**¿Por qué medimos antes de paralelizar?** Porque más hilos no garantizan menor tiempo. El filesystem, la caché, la carga y el tamaño del árbol cambian el resultado.

**¿Qué ocurre si un archivo desaparece durante el scan?** Se consulta el estado actual de la ruta; si la metadata deja de estar disponible se contabiliza como omitido y el resto del scan continúa.

## Referencias oficiales

- GCC: sitio y releases oficiales.
- CMake: documentación oficial.
- cppreference para APIs de biblioteca estándar.
- Microsoft Learn para MSVC y C++ en Windows.

## Cómo hablar de este proyecto en una entrevista

Explica cómo separaste filesystem, índice y persistencia; por qué mides antes de optimizar; cómo `std::jthread` y `std::stop_token` gobiernan lifetime/cancelación; cómo evitas un vector global compartido; qué diferencia multiplataforma expuso MSVC y cómo la corregiste sin debilitar tests.

## Siguiente paso

Resuelve la evaluación final sin receta. Si satisface la rúbrica y los gates ejecutables, conserva ThreadSeek como proyecto de portafolio y continúa usando el curso transversal de Git para versionarlo.
