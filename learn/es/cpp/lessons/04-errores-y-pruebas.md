# Lección 04 — Haz explícitos los errores y prueba comportamiento

## Objetivo

Distinguir errores de entrada, fallos tolerables del filesystem y regresiones mediante pruebas ejecutables.

Una raíz inexistente o que no es directorio invalida toda la operación y lanza `std::invalid_argument`. Entradas internas inaccesibles usan `skip_permission_denied` y `std::error_code` para que un subárbol problemático no destruya todo el índice.

`app/tests/indexer_tests.cpp` crea fixtures temporales y prueba cantidad, bytes, búsqueda y raíz inexistente. CTest ejecuta el binario de pruebas sin dependencias externas.

Durante la primera ejecución, un test esperaba 18 bytes aunque los fixtures sumaban 17. El código era correcto; corregimos la expectativa. TDD exige evidencia, no obediencia ciega a una aserción equivocada.

## Comprobación

```bash
cmake -S app -B app/build -DCMAKE_BUILD_TYPE=Release
cmake --build app/build --parallel
ctest --test-dir app/build --output-on-failure
```

Completa [Checkpoint 01 — Índice confiable](../exercises/checkpoint-01.md) y continúa con [Lección 05 — Separa descubrimiento del índice](05-separa-descubrimiento-del-indice.md).

## Referencias

Consulta la documentación de `std::filesystem`, `std::error_code` y excepciones de la biblioteca estándar de tu toolchain.
