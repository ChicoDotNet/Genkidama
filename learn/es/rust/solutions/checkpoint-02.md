# Solución de referencia — Checkpoint 02

La propiedad importante no es el nombre de una función: es **no reutilizar bytes cuya integridad actual no fue demostrada**.

Una solución razonable mantiene dos niveles de evidencia:

1. el manifest anterior permite localizar una entrada candidata por ruta;
2. la copia física actual debe seguir teniendo el mismo tamaño y SHA-256 esperado.

Sólo si ambas condiciones coinciden se incrementa `reused`. Si la copia del backup tiene el mismo tamaño pero otro hash, `update_backup` vuelve a copiar desde el origen y registra `copied += 1`.

La regresión de referencia sigue este patrón:

```rust
fs::write(source.path().join("data.txt"), b"ABCD").unwrap();
create_backup(source.path(), backup.path()).unwrap();
fs::write(backup.path().join("data.txt"), b"WXYZ").unwrap();

let report = update_backup(source.path(), backup.path()).unwrap();

assert_eq!(0, report.reused);
assert_eq!(1, report.copied);
assert!(verify_backup(backup.path(), &report.manifest)
    .unwrap()
    .is_valid());
```

El punto de usar contenido de igual longitud es evitar que una implementación que compare sólo `bytes` pase accidentalmente.

## Por qué no confiar sólo en el manifest

El manifest describe lo que BackupForge escribió y calculó en una ejecución anterior. No garantiza que el archivo no haya sido alterado después. Releer el destino antes de reutilizarlo evita propagar esa corrupción silenciosamente.

## Límite del diseño

`update_backup` mantiene **un backup actual**. No crea una cadena de snapshots inmutables ni aplica retención. Resolver historial, deduplicación global y consistencia transaccional entre versiones requiere otro diseño; no debe atribuirse a este checkpoint.

Vuelve a la [Lección 08](../lessons/08-invariantes-y-checkpoint-02.md) y compara comportamiento, no similitud textual.
