# Solución de referencia — Checkpoint 02

La implementación de referencia está en `app/include/threadseek/indexer.hpp` y `app/src/indexer.cpp`.

La frontera `discover_files` posee únicamente el recorrido del filesystem. `FileIndex` posee un `std::vector<FileRecord>` y puede construirse desde una raíz o desde registros existentes. `IndexStore` posee temporalmente los streams mediante RAII y persiste una cabecera versionada más los registros.

La prueba importante crea un índice, lo guarda, crea una instancia nueva desde el archivo y verifica que búsqueda y tamaños sobreviven. Otra prueba entrega una cabecera inválida y exige un error explícito.

La escritura temporal reduce el riesgo de publicar un índice parcialmente escrito. No pretende ser todavía una transacción durable frente a todos los fallos de energía/filesystem; esa diferencia debe poder explicarse en una revisión técnica.

Vuelve a [Lección 08](../lessons/08-fallos-de-persistencia.md).
