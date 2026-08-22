# Lección 01 — Compila y ejecuta ThreadSeek

## Objetivo

Compilar C++ real con CMake, ejecutar un binario nativo y entender qué problema resolverá ThreadSeek.

## Prerrequisitos

Ningún conocimiento previo de C++. Necesitas compilador y CMake instalados.

## Problema

Buscar manualmente archivos en árboles grandes es lento y poco repetible. Antes de optimizar nada necesitamos un programa que podamos compilar, ejecutar y observar.

## Concepto

C++ se compila antes de ejecutarse. CMake describe targets y requisitos; el compilador produce el ejecutable. En este proyecto el estándar queda fijado en C++23 y las advertencias del compilador se tratan como errores en targets propios.

[EN PANTALLA] Abre `app/CMakeLists.txt` y localiza `CMAKE_CXX_STANDARD`, `add_library` y `add_executable`.

## Demo

[EJECUTAR]

```bash
cd learn/es/cpp/app
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build --parallel
./build/threadseek . cpp
```

La CLI exige un directorio y acepta opcionalmente texto de búsqueda. Si la invocas sin argumentos devuelve código 2 y muestra el uso correcto.

## Código real

`src/main.cpp` mantiene la consola en el borde. Construye `threadseek::FileIndex`, ejecuta una consulta y presenta resultados. No contiene el algoritmo de recorrido.

## Explicación

- `argc` cuenta argumentos.
- `argv` contiene texto recibido por la aplicación.
- `const` expresa que una referencia/valor no se modificará desde ese punto.
- `try/catch` convierte errores del núcleo en un mensaje y código de salida útil para una CLI.

## Errores comunes

No ejecutes un binario viejo después de modificar código: recompila. No confundas errores de configuración CMake con errores del compilador. No desactives `-Werror` para ocultar una advertencia que sí puedes corregir.

## Buenas prácticas

Mantén el build fuera de `src/`; trata warnings como señal; evita lógica de negocio en `main`.

## Ejercicio

Modifica sólo el texto de salida para que también indique el término buscado. Recompila y comprueba que la búsqueda siga funcionando.

[PAUSA PARA EJERCICIO]

## Comprobación

```bash
ctest --test-dir build --output-on-failure
```

Las pruebas deben continuar verdes.

## Solución enlazada

La solución del checkpoint está separada en `../solutions/checkpoint-01.md`; no la abras todavía.

## Reto

Ejecuta ThreadSeek sobre una carpeta con subdirectorios y explica por qué el resultado cambia al usar una consulta vacía.

## Resumen

Ya compilaste, ejecutaste y probaste una aplicación C++ real. El siguiente paso es representar archivos con tipos del estándar.

## Siguiente paso

[Lección 02 — Modela archivos con tipos y `std::filesystem`](02-modela-archivos-con-tipos.md)

## Referencias

Consulta la documentación oficial de GCC y CMake enlazada desde el README del curso.
