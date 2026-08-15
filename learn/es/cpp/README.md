# Curso de C++ desde cero — Construye un indexador y buscador de archivos

C++ es un lenguaje compilado de propósito general usado cuando importan el rendimiento, la integración con sistemas nativos y el control explícito de recursos. **Sí puedes empezar desde cero aquí**: el curso introduce la sintaxis mientras construyes una sola aplicación real, **ThreadSeek**, un indexador/buscador de archivos que terminará usando concurrencia sólo después de tener comportamiento correcto y medible.

## Qué vas a construir

ThreadSeek recorre directorios, representa archivos con tipos claros, permite buscar por nombre, maneja fallos de filesystem y, en incrementos posteriores, persistirá índices, medirá tiempos y distribuirá trabajo entre varios hilos sin sacrificar determinismo.

## Qué necesitas

- Un compilador C++ moderno con soporte C++23. Este incremento fue probado con GCC 14.2.0.
- CMake 3.28 o posterior. Este incremento fue probado con CMake 3.31.6.
- VS Code es opcional; no necesitas un IDE específico.

En Linux puedes usar GCC/G++ y CMake de tu distribución. En Windows puedes usar MSVC Build Tools o un GCC moderno con CMake. Los comandos del curso se mantienen deliberadamente portables.

Al verificar este curso el 15 de agosto de 2026, GCC publicaba 16.1 como línea estable principal y CMake publicaba 4.4.2 como release estable actual. El baseline probado del curso es más conservador porque debe ser reproducible en el entorno real de validación, no una promesa basada sólo en el número más nuevo.

## Build, test y run

Desde `learn/es/cpp/app`:

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build --parallel
ctest --test-dir build --output-on-failure
./build/threadseek . txt
```

En PowerShell, el último comando normalmente será `./build/Release/threadseek.exe . txt` con un generador multi-configuración como Visual Studio, o `./build/threadseek.exe . txt` según el generador elegido.

## Avance

**4/17 lecciones.** El primer checkpoint ya deja un indexador pequeño, ejecutable y probado.

1. [Lección 01 — Compila y ejecuta ThreadSeek](lessons/01-compila-y-ejecuta-threadseek.md)
2. [Lección 02 — Modela archivos con tipos y `std::filesystem`](lessons/02-modela-archivos-con-tipos.md)
3. [Lección 03 — Busca sobre el índice con algoritmos simples](lessons/03-busca-sobre-el-indice.md)
4. [Lección 04 — Haz explícitos los errores y prueba comportamiento](lessons/04-errores-y-pruebas.md)
5. Próximo: separar descubrimiento, lectura e indexación.
6. Próximo: ownership y RAII en recursos reales.
7. Próximo: persistir y reconstruir el índice.
8. Próximo: checkpoint de arquitectura.
9. Próximo: medir antes de paralelizar.
10. Próximo: `std::thread`, tareas y partición de trabajo.
11. Próximo: sincronización mínima y datos compartidos.
12. Próximo: comparar versión secuencial vs multihilo.
13. Próximo: cancelación y progreso.
14. Próximo: robustez ante archivos que cambian.
15. Próximo: profiling y optimización basada en evidencia.
16. Próximo: hardening, portabilidad y entrega.
17. Próximo: evaluación final autónoma.

[Checkpoint 01 — Índice confiable](exercises/checkpoint-01.md) · [Solución de referencia](solutions/checkpoint-01.md)

## Qué sabrás hacer al terminar

Leer y escribir C++ moderno sencillo, comprender value semantics y ownership, separar I/O de lógica, usar la STL y `std::filesystem`, manejar errores, construir con CMake, probar comportamiento, depurar, medir rendimiento, trabajar con concurrencia sin carreras obvias y explicar las decisiones de arquitectura de una aplicación nativa pequeña.

## Empleabilidad

Estas habilidades aparecen en software de sistemas, motores, tooling, multimedia, bibliotecas nativas, infraestructura y productos donde latencia o consumo de recursos importan. C++ no debe venderse como una garantía de empleo junior: muchas vacantes piden fundamentos de sistemas, algoritmos, debugging y plataforma además del lenguaje.

## Preguntas frecuentes

**¿Necesito saber C antes?** No. C++ se enseña como lenguaje propio, no como “C con clases”.

**¿Por qué C++23 si todavía no usamos features avanzadas?** Porque fija un estándar moderno para el proyecto; las primeras lecciones usan herramientas sencillas y portables.

**¿Por qué no empezamos con punteros manuales?** Porque ownership manual no es un rito de iniciación. Primero usamos value types, RAII y contenedores estándar; los punteros aparecen cuando resuelven un problema real.

**¿Por qué aún no hay múltiples hilos?** Porque concurrencia sobre comportamiento incorrecto sólo produce errores más rápidos. Primero construimos una línea base correcta y medible.

## Glosario inicial

- **Compilador:** transforma código C++ en un binario nativo.
- **STL:** biblioteca estándar de contenedores, algoritmos, strings y utilidades.
- **RAII:** vincula la vida de un recurso con la vida de un objeto.
- **Value semantics:** tratar objetos como valores que pueden copiarse/moverse con reglas claras.
- **Thread:** flujo de ejecución que puede correr concurrentemente con otros.

## Referencias oficiales

- GCC: sitio y releases oficiales.
- CMake: documentación oficial.
- ISO C++ / isocpp.org para recursos del ecosistema del estándar.

## Cómo hablar de este proyecto en una entrevista

No digas sólo “hice un buscador”. Explica cómo separaste filesystem, modelo e interfaz; qué casos de error probaste; por qué mediste una versión secuencial antes de agregar threads; qué datos compartiste entre hilos y cómo evitaste convertir sincronización en estado global mutable.

## Siguiente paso

Completa las cuatro primeras lecciones y el checkpoint. Después haremos durable el índice antes de introducir concurrencia.
