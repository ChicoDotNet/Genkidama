# Lección 05 — Separa descubrimiento del índice

Hasta ahora `FileIndex` hacía dos trabajos: recorrer el filesystem y representar un índice consultable. Extraemos `discover_files(root)` para que la frontera de I/O produzca `FileRecord` y el índice pueda construirse también desde datos ya existentes.

La separación no busca presumir arquitectura: habilita una necesidad concreta del siguiente paso, reconstruir un índice sin volver a tocar el disco original.

Observa que `std::vector<FileRecord>` usa value semantics. El dueño del vector es explícito y no necesitamos `new`, `delete` ni punteros compartidos.

## Práctica

Lee `discover_files` y `FileIndex(std::vector<FileRecord>)`. Explica qué parte puede fallar por filesystem y qué parte sólo ordena/consulta valores.

Siguiente: [Lección 06 — RAII en recursos reales](06-raii-en-recursos-reales.md).
