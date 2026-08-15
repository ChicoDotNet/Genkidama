# Lección 03 — Busca sobre el índice con algoritmos simples

## Objetivo

Implementar una consulta útil sin mezclar búsqueda con recorrido de disco ni salida de consola.

## Problema

Recorrer el disco cada vez que cambia el texto buscado haría imposible razonar después sobre rendimiento y concurrencia. Necesitamos separar “construir índice” de “consultar índice”.

## Concepto

`FileIndex::search(std::string_view)` recibe una vista no propietaria del término y devuelve nuevos `FileRecord` para las coincidencias. La implementación normaliza ASCII y usa `std::string::contains`, disponible con el estándar configurado.

## Demo

[EJECUTAR]

```bash
./app/build/threadseek . txt
```

Prueba `TXT`, `txt` y una consulta vacía. La búsqueda actual ignora mayúsculas sólo para ASCII; no prometemos todavía normalización Unicode completa.

## Código real

```cpp
for (const auto& record : files_) {
    const auto filename = ascii_lower(record.path.filename().string());
    if (filename.contains(normalized_query)) {
        matches.push_back(record);
    }
}
```

La complejidad es lineal respecto al número de archivos indexados. Eso está bien como baseline: primero necesitamos una referencia correcta que luego podamos medir.

## Errores comunes

No optimices con threads antes de medir. No devuelvas referencias a un vector local. No uses `tolower` directamente con un `char` potencialmente negativo; la conversión a `unsigned char` evita comportamiento problemático en valores fuera de ASCII básico.

## Buenas prácticas

Haz que la consulta sea determinista. Declara las limitaciones de encoding. Mantén búsqueda libre de I/O para que sea fácil de probar.

## Ejercicio

Añade una prueba para una consulta que no tenga coincidencias y comprueba que devuelve un vector vacío sin lanzar excepción.

## Comprobación

Las pruebas actuales cubren consulta sin distinguir mayúsculas y consulta vacía.

```bash
ctest --test-dir app/build --output-on-failure
```

## Solución enlazada

Consulta `../solutions/checkpoint-01.md` sólo después de intentar el checkpoint.

## Reto

¿Conviene buscar por path completo o sólo por nombre? Escribe una regla de producto concreta antes de cambiar el comportamiento.

## Resumen

ThreadSeek ya separa descubrimiento en disco de consultas en memoria. Esa frontera será clave cuando paralelicemos trabajo.

## Siguiente paso

[Lección 04 — Haz explícitos los errores y prueba comportamiento](04-errores-y-pruebas.md)

## Referencias

Revisa la documentación de `std::string_view`, `std::vector` y tu implementación de la STL.
