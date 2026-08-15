# Solución de referencia — Checkpoint 01

Una solución razonable crea dos archivos, ejecuta `create_backup`, elimina uno dentro del destino y después carga el manifest para llamar `verify_backup`. La aserción importante es que `checked` conserva el número total de entradas y `mismatches` contiene exactamente la ruta eliminada.

Ejemplo de dirección:

```rust
let manifest = load_manifest(backup.path()).unwrap();
let result = verify_backup(backup.path(), &manifest).unwrap();
assert_eq!(2, result.checked);
assert_eq!(vec!["missing.txt"], result.mismatches);
```

El objetivo no es copiar nombres exactos sino proteger el contrato: ausencia es mismatch de integridad, no un éxito parcial ni un panic.

SHA-256 detecta cambios de contenido con gran fiabilidad, pero si un atacante puede reemplazar simultáneamente los archivos y `manifest.json`, puede recalcular los hashes. Autenticidad requiere una frontera adicional, por ejemplo firma/MAC y gestión segura de claves; eso se abordaría como hardening posterior, no se finge en este checkpoint.
