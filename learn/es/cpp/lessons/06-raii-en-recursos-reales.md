# Lección 06 — RAII en recursos reales

RAII significa que la vida de un recurso queda ligada a la vida de un objeto. `std::ifstream` y `std::ofstream` abren archivos al construirse y los cierran al salir del scope, incluso si una excepción interrumpe el flujo.

ThreadSeek usa ese mecanismo al persistir el índice. No escribimos `close()` en cada rama ni administramos handles manualmente: dejamos que los tipos estándar sean dueños del recurso.

Esto también explica por qué no introducimos punteros manuales sólo para “aprender memoria”. C++ moderno empieza por ownership visible y tipos que limpian sus recursos automáticamente.

## Práctica

Identifica los scopes de `IndexStore::save`. ¿Por qué conviene cerrar el archivo temporal antes de intentar renombrarlo, especialmente pensando en Windows?

Siguiente: [Lección 07 — Persiste y reconstruye el índice](07-persiste-y-reconstruye-el-indice.md).
