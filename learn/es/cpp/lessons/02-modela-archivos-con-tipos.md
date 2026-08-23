# Lección 02 — Modela archivos con tipos y `std::filesystem`

## Objetivo

Usar `struct`, clases, vectores y `std::filesystem` para convertir archivos reales en datos que ThreadSeek pueda consultar.

## Problema

Un path impreso en consola no basta para construir un buscador. Necesitamos conservar al menos ruta y tamaño de cada archivo con tipos que expresen intención.

## Concepto

`FileRecord` es un value type pequeño: contiene `std::filesystem::path` y `std::uintmax_t`. `FileIndex` posee un `std::vector<FileRecord>`; cuando el objeto muere, el vector libera su memoria automáticamente. Eso es parte del modelo RAII que evita empezar con `new`/`delete` manuales.

## Demo

[EN PANTALLA] Revisa `app/include/threadseek/indexer.hpp`.

```cpp
struct FileRecord {
    std::filesystem::path path;
    std::uintmax_t size_bytes{};
};
```

La interfaz pública tiene comentarios Doxygen para que propósito, errores y garantías sean visibles desde el header.

## Código real

El constructor de `FileIndex` usa `recursive_directory_iterator` con `skip_permission_denied`, acepta sólo archivos regulares y ordena el resultado por representación genérica de la ruta.

Ordenar aquí no es cosmético: una salida determinista hace que pruebas, debugging y comparaciones sean reproducibles.

## Errores comunes

No representes paths concatenando strings con `/` o `\\`; `std::filesystem::path` conoce diferencias de plataforma. No guardes punteros a entradas del iterador: su vida no representa la vida del índice.

## Buenas prácticas

Prefiere value types y contenedores estándar. Mantén headers autocontenidos. Documenta las superficies públicas antes de hacerlas crecer.

## Ejercicio

Agrega a `FileRecord` una propiedad booleana que indique si la extensión del archivo es `.txt`. Actualiza el constructor del índice y una prueba. Después piensa si esa propiedad derivada merece quedar almacenada permanentemente.

## Comprobación

```bash
cmake --build app/build --parallel
ctest --test-dir app/build --output-on-failure
```

## Solución enlazada

El checkpoint final del bloque tiene solución separada en `../solutions/checkpoint-01.md`.

## Reto

Explica qué ownership existe para cada `FileRecord` y por qué no necesitamos liberar cada elemento manualmente.

## Resumen

ThreadSeek ya transforma filesystem en un modelo pequeño, ordenado y poseído claramente.

## Siguiente paso

[Lección 03 — Busca sobre el índice con algoritmos simples](03-busca-sobre-el-indice.md)

## Referencias

Usa la documentación de la biblioteca estándar de tu toolchain para `std::filesystem` y `std::vector`.
