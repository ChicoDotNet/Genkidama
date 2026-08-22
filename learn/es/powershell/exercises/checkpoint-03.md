# Checkpoint 03 — Detecta una regresión entre auditorías

Has recibido dos reportes de la misma estación: un baseline y una ejecución actual. Implementa una prueba que demuestre que WorkstationAudit puede distinguir un hallazgo nuevo de un cambio de severidad.

## Requisitos
1. Construye dos objetos de auditoría con `ComputerName = 'checkpoint-03'`.
2. En ambos incluye `storage.free-space` para la unidad `C`, pero cambia `Info` → `Warning`.
3. Añade sólo en la auditoría actual un finding `execution.elevated`.
4. Ejecuta `Compare-WorkstationAudit`.
5. Demuestra con Pester que `Added = 1`, `Changed = 1` y `Resolved = 0`.
6. Explica en un comentario por qué comparar el texto de `Message` sería más frágil que usar una identidad estable.

No consultes hardware real. La evidencia del checkpoint debe ser completamente reproducible.

Después de intentarlo: [solución de referencia](../solutions/checkpoint-03.md).
