# Lección 05 — De copia completa a actualización incremental

## Qué vas a conseguir
Convertirás BackupForge en una herramienta que reutiliza archivos ya correctos y sólo vuelve a copiar lo que cambió.

## Antes de empezar
Completa la [Lección 04](04-verificacion-y-checkpoint-01.md) y conserva un backup válido.

## El problema
Copiar todo en cada ejecución funciona, pero desperdicia I/O. La optimización fácil —confiar sólo en nombre, tamaño o fecha— puede reutilizar contenido incorrecto.

## Concepto
La incrementalidad de BackupForge es conservadora: compara el nuevo SHA-256 del origen con el manifest anterior y además verifica que el archivo ya respaldado siga coincidiendo. Sólo entonces lo cuenta como `reused`.

`IncrementalReport` hace observable la decisión:

- `reused`: archivos que no necesitaron escritura;
- `copied`: archivos nuevos, modificados o reparados;
- `removed`: archivos que ya no existen en el origen.

## Demostración
[DEMO] Crea dos archivos, ejecuta `create`, modifica uno y después:

```bash
cargo run -- update ./origen ./backup
```

La salida debe distinguir reutilizados y copiados.

## Código real
`update_backup` mantiene el filesystem en la frontera, pero expresa el resultado con un tipo público documentado. Usa `BTreeMap` para localizar el estado previo por ruta y `HashSet` para detectar cuáles rutas siguen existiendo.

## Qué acaba de pasar
Optimizamos una operación visible sin debilitar el contrato de integridad. El checksum sigue siendo la autoridad sobre el contenido.

## Errores comunes
- Considerar igual un archivo porque tiene el mismo tamaño.
- Saltar el hash del backup existente y propagar corrupción.
- Borrar primero el manifest anterior antes de construir el nuevo estado.
- Llamar “deduplicación” a algo que sólo evita reescrituras dentro de un backup.

## Buenas prácticas
Mide y reporta qué se reutilizó. Si una optimización no puede explicarse ni probarse, todavía no es una optimización confiable.

## Tu turno
Agrega un tercer archivo, ejecuta `update` dos veces sin modificar nada y explica por qué la segunda ejecución debe reportar tres reutilizados y cero copiados.

## Cómo comprobar
```bash
cargo test incremental_update_reuses_unchanged_and_copies_modified_files
cargo run -- update ./origen ./backup
cargo run -- verify ./backup
```

## Solución enlazada
La implementación canónica está en [`../app/src/lib.rs`](../app/src/lib.rs). Léela después de formular tu propia estrategia.

## Reto adicional
Explica qué cambiaría si quisieras snapshots históricos inmutables en vez de actualizar un único backup.

## Resumen
BackupForge ya puede hacer trabajo incremental sin confiar en metadata débil.

## Siguiente paso
Continúa con la [Lección 06 — Corrupción, borrados y consistencia](06-corrupcion-borrados-y-consistencia.md).

## Referencias
- https://doc.rust-lang.org/std/collections/struct.BTreeMap.html
- https://doc.rust-lang.org/std/collections/struct.HashSet.html
- https://doc.rust-lang.org/std/fs/fn.copy.html
