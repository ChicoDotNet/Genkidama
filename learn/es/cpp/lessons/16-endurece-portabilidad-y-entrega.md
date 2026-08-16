# Lección 16 — Endurece portabilidad y entrega

## Objetivo

Demostrar que ThreadSeek no depende accidentalmente de un solo compilador o sistema operativo.

## Problema

Código que compila en la laptop del autor puede depender de extensiones, rutas, separadores o comportamientos particulares. C++ portable requiere probar más de una implementación, no sólo afirmar que el estándar lo permite.

## Concepto

El workflow de Learn C++ construye y prueba el mismo CMake project en GCC y Clang sobre Linux y MSVC sobre Windows. Los warnings siguen tratándose como errores. El smoke test usa rutas nativas por plataforma y CTest mantiene el contrato común.

El primer pase multiplataforma encontró una diferencia real: `std::filesystem::directory_entry` podía conservar metadata cacheada en Windows después de que el archivo se eliminara. La solución no fue excluir MSVC ni cambiar la expectativa; ThreadSeek pasó a consultar el estado actual mediante las funciones libres `std::filesystem::is_regular_file(path, error)` y `file_size(path, error)`.

## Aplicación real

Antes de entregar una biblioteca o CLI nativa, define la matriz de plataformas soportadas. Si una plataforma no puede validarse, declárala como no verificada en vez de asumir compatibilidad.

## Errores comunes

- `#ifdef` como primera solución;
- concatenar rutas con `/` o `\\` manualmente;
- silenciar warnings diferentes entre compiladores;
- afirmar soporte Windows sin ejecutar MSVC;
- convertir una diferencia de implementación en un test más débil sin entender la causa.

## Ejercicio

Provoca deliberadamente un warning portable y observa cómo la matriz lo rechaza. Corrige la causa sin bajar `/W4`, `-Wall`, `-Wextra`, `-Wpedantic` ni quitar `-Werror`/`/WX`.

## Comprobación

El incremento está listo sólo cuando Linux/GCC, Linux/Clang y Windows/MSVC compilan, ejecutan CTest y completan el smoke de CLI correspondiente.

## Reflexión

¿Qué significa realmente “portable”: compilar, pasar tests o ofrecer exactamente la misma experiencia operacional?

## Siguiente paso

Completa el [Checkpoint 04 — Operación robusta](../exercises/checkpoint-04.md) y continúa con la [Lección 17 — Evaluación final: entrega ThreadSeek](17-evaluacion-final.md).
