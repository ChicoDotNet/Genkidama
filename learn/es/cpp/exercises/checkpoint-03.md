# Checkpoint 03 — Concurrencia medible y determinista

Extiende ThreadSeek sin copiar la solución.

1. Construye un fixture con al menos tres subdirectorios y archivos de tamaños distintos.
2. Ejecuta descubrimiento secuencial y paralelo con 1, 2 y 4 workers.
3. Demuestra con pruebas que rutas, tamaños y orden son idénticos.
4. Registra tiempos observados, pero no conviertas «paralelo debe ser más rápido» en un assertion.
5. Explica por qué los workers producen lotes locales en lugar de escribir todos sobre un vector compartido.
6. Fuerza `worker_count = 0` y conserva el error explícito.

Entrega código, resultados de `ctest` y una breve conclusión sobre cuándo el paralelismo parece útil en tu máquina.

[Solución de referencia](../solutions/checkpoint-03.md)
