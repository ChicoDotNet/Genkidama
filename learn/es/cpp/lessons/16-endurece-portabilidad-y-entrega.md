# Lección 16 — Endurece portabilidad y entrega

## Objetivo

Demostrar que ThreadSeek no depende accidentalmente de un solo compilador o sistema operativo.

## Problema

Código que compila en la laptop del autor puede depender de extensiones, rutas, separadores o comportamientos particulares. C++ portable requiere probar más de una implementación, no sólo afirmar que el estándar lo permite.

## Concepto

El workflow de Learn C++ construye y prueba el mismo CMake project en GCC y Clang sobre Linux y MSVC sobre Windows. Los warnings siguen tratándose como errores. El smoke test usa rutas nativas por plataforma y CTest mantiene el contrato común.

No buscamos que cada compilador genere el mismo binario. Buscamos que el comportamiento público, pruebas y build descritos por el curso sean reproducibles.

## Aplicación real

Antes de entregar una biblioteca o CLI nativa, define la matriz de plataformas soportadas. Si una plataforma no puede validarse, declárala como no verificada en vez de asumir compatibilidad.

## Errores comunes

- `#ifdef` como primera solución;
- concatenar rutas con `/` o `\\` manualmente;
- silenciar warnings diferentes entre compiladores;
- afirmar soporte Windows sin ejecutar MSVC;
- introducir dependencias sólo para evitar aprender CMake o STL.

## Ejercicio

Provoca deliberadamente un warning portable y observa cómo la matriz lo rechaza. Corrige la causa sin bajar `/W4`, `-Wall`, `-Wextra`, `-Wpedantic` ni quitar `-Werror`/`/WX`.

## Comprobación

El incremento está listo sólo cuando Linux/GCC, Linux/Clang y Windows/MSVC compilan, ejecutan CTest y completan el smoke de CLI correspondiente.

## Reflexión

¿Qué significa realmente “portable”: compilar, pasar tests o ofrecer exactamente la misma experiencia operacional?

## Siguiente paso

Completa el [Checkpoint 04 — Operación robusta](../exercises/checkpoint-04.md). Después quedará la evaluación final autónoma de la lección 17.
