# Solución de referencia — Evaluación final Rust

> Consulta esta referencia sólo después de completar un intento serio. No existe una única solución correcta.

## Dirección de diseño

Una solución razonable mantiene la validación de rutas dentro del contrato compartido del manifest y añade la restauración selectiva como API pública de la biblioteca, dejando a la CLI únicamente el parsing de argumentos, mensajes y código de proceso.

La operación selectiva debería reutilizar las garantías existentes en vez de crear un segundo sistema de integridad. Una dirección conservadora es validar el manifest, localizar exactamente la entrada solicitada, verificar el backup antes de escribir y copiar después sólo ese archivo. Para backups grandes podría medirse y justificarse otra política, pero cambiar de “verificación completa antes de restore” a “sólo verificar el archivo pedido” es un tradeoff que debe ser explícito, no accidental.

## Historia A — Restauración selectiva

Una API posible podría tener una forma equivalente a:

```rust
pub fn restore_file(
    backup: &Path,
    destination: &Path,
    manifest: &Manifest,
    relative_path: &Path,
) -> Result<PathBuf, BackupError>
```

No es obligatorio usar ese nombre o retorno. Lo importante es proteger estas propiedades:

1. el manifest se valida antes de confiar en sus rutas;
2. la ruta pedida debe representar exactamente una entrada declarada;
3. la política de integridad se ejecuta antes de escribir;
4. sólo después se crean directorios y se copia el archivo;
5. el caller recibe un `Result` y decide cómo mostrar/finalizar.

Una regresión fuerte prepara un backup válido, restaura `docs/manual.txt` y comprueba bytes y ubicación. Otra corrompe el backup y verifica que el destino solicitado no aparece.

## Historia B — Rutas equivalentes

La validación existente rechaza rutas absolutas y `ParentDir`, pero una ruta con `Component::CurDir` puede ser semánticamente redundante. Una corrección pequeña puede rechazar cualquier componente `CurDir` además de `ParentDir`.

Conceptualmente:

```rust
if path.components().any(|component| {
    matches!(
        component,
        std::path::Component::ParentDir | std::path::Component::CurDir
    )
}) {
    return Err(BackupError::InvalidManifest(...));
}
```

La prueba de regresión debe demostrar que `docs/./manual.txt` ya no se acepta como una ruta distinta de `docs/manual.txt`.

No es necesario “normalizar y aceptar” una entrada ambigua. Para un formato persistido y verificable, rechazar una representación no canónica suele ser más fácil de razonar que transformarla silenciosamente.

## Historia C — Errores y documentación

Si se agrega una API pública, rustdoc debe explicar propósito, parámetros, retorno, errores y efectos de filesystem. Los errores de ruta no declarada pueden modelarse con una nueva variante estructurada de `BackupError` o con una variante existente si el diagnóstico continúa siendo inequívoco.

No uses `unwrap()` para errores que dependan de archivos suministrados por el operador.

## Historia D — Tooling

El cierre esperado sigue siendo:

```bash
bash tools/verify.sh
```

Si rustfmt o Clippy piden un cambio, se corrige el código; no se retira el gate. Las pruebas nuevas deben ser offline y crear sus propios temporales/fixtures.

## Historia E — Fuentes oficiales

Una nota válida puede relacionar `std::path::Component::CurDir` con el rechazo de rutas no canónicas y `Result` con la propagación de errores recuperables. También puede documentarse `std::fs::copy`, rustdoc o `cargo test` si sustentan decisiones reales.

## Historia F — Repositorio remoto multi-writer

Una dirección razonable empieza por sustituir el filesystem directo detrás de una frontera de almacenamiento con operaciones explícitas como escribir contenido temporal, leer/verificar, publicar una versión y listar versiones.

Publicar un snapshot remoto necesita algo más fuerte que “copiar y luego renombrar” si el backend no ofrece rename atómico. Pueden requerirse objetos inmutables, IDs únicos y una operación de commit/manifest con precondiciones o compare-and-swap. Para múltiples escritores también deben tratarse colisiones, idempotencia y consistencia del listado.

SHA-256 detecta cambios accidentales, pero un atacante que puede reemplazar bytes y manifest puede recalcular hashes. Autenticidad requeriría firma/MAC y administración de claves, además de controles del almacenamiento.

## Defensa de entrevista

Una respuesta fuerte distingue:

- reglas/validación del manifest;
- cálculo y comparación de checksums;
- I/O de filesystem;
- orquestación CLI;
- integridad accidental frente a autenticidad adversarial.

También reconoce que una restauración selectiva segura tiene un costo de verificación que quizá sea alto en backups enormes. Antes de reducir esa garantía conviene medir tamaño, tiempo de hashing y escenarios de recuperación, y luego documentar una política explícita.

Vuelve a [`../exercises/rubrica-final.md`](../exercises/rubrica-final.md) y puntúa tu solución por evidencia y explicación, no por similitud de líneas con esta referencia.
