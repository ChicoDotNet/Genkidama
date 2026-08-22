# Lección 08 — Diseña fallos de persistencia explícitos

Persistir introduce nuevos failure modes: archivo inexistente, permisos, escritura incompleta o contenido corrupto. ThreadSeek no convierte esos casos en un índice vacío silencioso; `IndexStore::load` lanza `std::runtime_error` cuando no puede confiar en los datos.

Esto conserva una distinción útil: una raíz inválida al descubrir archivos es un error de entrada (`std::invalid_argument`); un índice persistido ilegible o corrupto es un fallo de infraestructura/formato (`std::runtime_error`).

No necesitamos una jerarquía compleja de excepciones todavía. La granularidad debe crecer cuando el consumidor necesite reaccionar de manera diferente.

## Práctica

Rompe la cabecera del archivo persistido y ejecuta la prueba `rejects_corrupt_index`. Después completa el [Checkpoint 02 — Índice durable](../exercises/checkpoint-02.md).

## Siguiente paso

En la [lección 09 — Mide la línea base antes de paralelizar](09-mide-la-linea-base.md) instrumentaremos la versión secuencial antes de introducir múltiples hilos.
