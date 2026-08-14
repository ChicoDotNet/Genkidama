# Lección 07 — Restore seguro antes de escribir

## Qué vas a conseguir
Restaurarás archivos únicamente después de demostrar que el backup completo coincide con su manifest.

## Antes de empezar
Completa la [Lección 06](06-corrupcion-borrados-y-consistencia.md).

## El problema
Una copia que puede verificarse pero no restaurarse todavía no completa el ciclo operativo. Peor aún: restaurar mientras verificas puede dejar media salida escrita antes de descubrir una corrupción tardía.

## Concepto
BackupForge separa dos fases:

1. validar el manifest y verificar todos los archivos;
2. sólo si todo coincide, crear directorios y copiar la salida.

Una falla de integridad se expresa como `BackupError::Integrity(Vec<String>)`, no como texto ambiguo ni `panic!`.

## Demostración
[DEMO] Ejecuta:

```bash
cargo run -- restore ./backup ./restaurado
```

Luego altera un archivo del backup y repite hacia un directorio vacío. La operación debe fallar antes de escribir ese archivo.

## Código real
`restore_backup` recibe `&Manifest`: lo toma prestado porque no necesita apropiarse del inventario. La función vuelve a validar el manifest aunque el caller pudiera haber construido el valor manualmente.

## Qué acaba de pasar
El límite de confianza quedó más claro: una API pública no asume que sus argumentos pasaron por otra API concreta.

## Errores comunes
- Restaurar archivo por archivo y verificar después de copiar cada uno.
- Usar `unwrap()` en errores recuperables de filesystem.
- Confiar en un `Manifest` construido en memoria sólo porque tiene el tipo correcto.
- Borrar archivos ajenos del destino sin una política explícita.

## Buenas prácticas
Haz que el error ocurra antes del efecto irreversible cuando sea razonable. Mantén el restore conservador: BackupForge escribe los archivos declarados y no borra contenido ajeno del destino.

## Tu turno
Escribe una prueba que altere el último archivo del manifest y demuestre que el directorio de restore permanece sin ese contenido.

## Cómo comprobar
```bash
cargo test restore_copies_only_after_successful_verification
cargo test restore_refuses_corrupt_backup_before_writing_output
```

## Solución enlazada
Compara tu enfoque con `restore_backup` en [`../app/src/lib.rs`](../app/src/lib.rs).

## Reto adicional
Diseña, sin implementar, una estrategia de restore atómico hacia un directorio temporal + rename. Identifica qué cambia entre archivos y directorios.

## Resumen
BackupForge ya cubre creación, actualización incremental, verificación y restauración defensiva.

## Siguiente paso
Continúa con la [Lección 08 — Invariantes y Checkpoint 02](08-invariantes-y-checkpoint-02.md).

## Referencias
- https://doc.rust-lang.org/std/fs/fn.create_dir_all.html
- https://doc.rust-lang.org/std/result/enum.Result.html
- https://doc.rust-lang.org/book/ch04-02-references-and-borrowing.html
