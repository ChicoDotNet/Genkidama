# Lección 06 — Corrupción, borrados y consistencia

## Qué vas a conseguir
Harás que la actualización incremental repare un destino corrupto y elimine del backup archivos que ya no pertenecen al origen.

## Antes de empezar
Completa la [Lección 05](05-actualizacion-incremental.md).

## El problema
Un algoritmo incremental puede ser rápido y estar equivocado. Si el archivo respaldado fue alterado después de la última ejecución, reutilizarlo porque el manifest dice que era correcto propaga corrupción. Si un archivo desapareció del origen, dejarlo para siempre también falsea el estado actual.

## Concepto
La regla es: **el manifest anterior ayuda a localizar candidatos, pero no sustituye la evidencia actual**. Para reutilizar, BackupForge comprueba tamaño y SHA-256 del destino. Para borrar, compara el conjunto de rutas del manifest anterior contra las rutas actuales.

## Demostración
[DEMO] Crea un backup de `data.txt`, modifica únicamente la copia respaldada conservando el mismo tamaño y ejecuta `update`. Debe reportar una copia, no una reutilización, y `verify` debe volver a quedar verde.

Después elimina un archivo del origen y repite `update`: la salida debe incrementar `removed` y el nuevo manifest ya no debe mencionarlo.

## Código real
Las regresiones `incremental_update_repairs_corrupt_destination_instead_of_reusing_it` e `incremental_update_removes_files_no_longer_present_in_source` protegen ambos comportamientos.

## Qué acaba de pasar
El backup representa el estado actual del origen y puede autorreparar una copia alterada cuando el origen correcto sigue disponible.

## Errores comunes
- Usar el manifest como prueba de que el archivo físico sigue íntegro.
- Conservar archivos obsoletos sin declararlo como política de retención.
- Borrar rutas que nunca estuvieron gobernadas por el manifest.
- Ocultar una lectura fallida y tratarla como “archivo cambiado”.

## Buenas prácticas
Distingue claramente estado deseado, evidencia previa y estado físico actual. Borra sólo aquello que el backup anterior administraba.

## Tu turno
Crea tres archivos, elimina uno del origen, corrompe otro en el backup y deja el tercero intacto. Predice `reused`, `copied` y `removed` antes de ejecutar.

## Cómo comprobar
```bash
cargo test incremental_update_repairs_corrupt_destination_instead_of_reusing_it
cargo test incremental_update_removes_files_no_longer_present_in_source
cargo run -- verify ./backup
```

## Solución enlazada
Consulta la implementación de `update_backup` en [`../app/src/lib.rs`](../app/src/lib.rs) sólo después de escribir tu predicción.

## Reto adicional
¿Qué garantía adicional necesitarías para que una interrupción a mitad de `update` dejara siempre un backup anterior completamente utilizable?

## Resumen
Incremental no significa “confiar más”; significa reducir escrituras conservando evidencia fuerte.

## Siguiente paso
Continúa con la [Lección 07 — Restore seguro antes de escribir](07-restore-seguro.md).

## Referencias
- https://doc.rust-lang.org/std/fs/fn.remove_file.html
- https://doc.rust-lang.org/std/fs/fn.metadata.html
