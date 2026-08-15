# Checkpoint 01 — Verificación defendible

Sin modificar la CLI, agrega una regresión que demuestre que un archivo respaldado eliminado después de crear el backup aparece en `Verification.mismatches` y que los demás archivos todavía se verifican.

Condiciones:

- no uses `unwrap()` en código de biblioteca nuevo;
- no conviertas el error en un `bool` ambiguo;
- conserva orden determinista;
- ejecuta `cargo fmt --check`, `cargo clippy --all-targets --all-features -- -D warnings` y `cargo test`.

Después explica por qué el checksum detecta corrupción accidental pero no autentica el manifest frente a un atacante que puede reemplazar ambos.
