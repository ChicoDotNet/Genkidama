# Lección 07 — Persiste y reconstruye el índice

Un índice útil no debería exigir un recorrido completo cada vez que inicia el programa. `IndexStore` guarda la lista de rutas y tamaños y luego la reconstruye mediante `FileIndex(std::vector<FileRecord>)`.

El formato empieza con `THREADSEEK\t1`: una cabecera/versionado mínimo permite rechazar datos que no sabemos interpretar. Las rutas se escriben con `std::quoted`, de modo que espacios y escapes no rompan el registro.

La escritura usa un archivo temporal y después reemplaza el destino. Así evitamos considerar válido un archivo que quedó a medias durante la escritura.

## Práctica

Ejecuta las pruebas y localiza `persists_and_reconstructs_index`. La propiedad importante no es “se creó un archivo”, sino que una instancia nueva recupera tamaño y capacidad de búsqueda sin escanear la raíz.

Siguiente: [Lección 08 — Diseña fallos de persistencia explícitos](08-fallos-de-persistencia.md).
