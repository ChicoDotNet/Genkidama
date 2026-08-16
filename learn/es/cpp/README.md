# Curso de C++ desde cero — Construye un indexador y buscador de archivos

C++ es un lenguaje compilado de propósito general usado cuando importan rendimiento, integración nativa y control explícito de recursos. El curso construye una sola aplicación real: **ThreadSeek**, un indexador/buscador que introduce concurrencia sólo después de establecer comportamiento correcto, durable y medible.

## Qué vas a construir

ThreadSeek recorre directorios, representa archivos con tipos claros, busca por nombre, persiste/reconstruye su índice y ahora puede comparar descubrimiento secuencial contra una versión paralela acotada sin sacrificar determinismo.

## Qué necesitas

- Compilador moderno con C++23. Baseline probado: GCC 14.2.0.
- CMake 3.28 o posterior. Baseline probado: CMake 3.31.6.
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

**12/17 lecciones.** Ya existe una línea base secuencial durable y una implementación paralela acotada que conserva el mismo resultado observable.

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
13. Próximo: cancelación y progreso.
14. Próximo: robustez ante archivos que cambian.
15. Próximo: profiling y optimización basada en evidencia.
16. Próximo: hardening, portabilidad y entrega.
17. Próximo: evaluación final autónoma.

[Checkpoint 01 — Índice confiable](exercises/checkpoint-01.md) · [Solución](solutions/checkpoint-01.md)

[Checkpoint 02 — Índice durable](exercises/checkpoint-02.md) · [Solución](solutions/checkpoint-02.md)

[Checkpoint 03 — Concurrencia medible y determinista](exercises/checkpoint-03.md) · [Solución](solutions/checkpoint-03.md)

## Qué sabrás hacer al terminar

Leer C++ moderno, comprender value semantics y ownership, usar RAII y STL, separar I/O de lógica, manejar errores, construir con CMake, probar comportamiento, medir rendimiento y trabajar con concurrencia sin carreras obvias.

## Empleabilidad

Estas habilidades aparecen en software de sistemas, motores, tooling, multimedia, bibliotecas nativas e infraestructura. El curso no promete empleo: una vacante junior puede exigir además algoritmos, debugging y fundamentos de plataforma.

## Preguntas frecuentes

**¿Necesito saber C antes?** No. C++ se enseña como lenguaje propio.

**¿Por qué no empezamos con punteros manuales?** Porque ownership manual no es un rito de iniciación. Primero usamos valores, RAII y contenedores estándar.

**¿Por qué medimos antes de paralelizar?** Porque más hilos no garantizan menor tiempo. El filesystem, la caché, la carga y el tamaño del árbol cambian el resultado.

**¿Por qué no usamos un mutex global?** Porque cada worker produce un lote local y el hilo coordinador combina los resultados después del join; eliminar estado mutable compartido simplifica el razonamiento.

## Referencias oficiales

- GCC: sitio y releases oficiales.
- CMake: documentación oficial.
- ISO C++ / isocpp.org para recursos del ecosistema del estándar.

## Cómo hablar de este proyecto en una entrevista

Explica cómo separaste filesystem, índice y persistencia; por qué mides una baseline antes de optimizar; cómo `std::jthread` mantiene ownership de los workers; cómo evitaste un vector compartido durante el escaneo; y por qué el resultado final se ordena para conservar determinismo.

## Siguiente paso

Completa Checkpoint 03. Después agregaremos cancelación/progreso, robustez ante archivos que cambian y profiling antes del hardening final.
