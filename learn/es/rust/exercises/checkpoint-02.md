# Checkpoint 02 — Incrementalidad que no propaga corrupción

## Escenario

BackupForge ya puede actualizar un backup existente. Debes proteger una propiedad operativa: **un archivo que parece sin cambios según el manifest anterior, pero cuya copia física fue alterada, nunca puede contarse como reutilizado**.

## Tu misión

Trabaja sobre la aplicación canónica y añade o mejora una regresión que cubra este flujo:

1. crea un archivo de origen y un backup válido;
2. altera únicamente la copia dentro del backup, conservando el mismo tamaño;
3. ejecuta la actualización incremental sin modificar el origen;
4. demuestra que el reporte registra el archivo como `copied`, no `reused`;
5. demuestra que el backup vuelve a verificar correctamente;
6. explica por qué comparar sólo ruta + bytes + manifest anterior habría sido incorrecto.

No cambies el manifest manualmente para hacer pasar la prueba. La reparación debe ocurrir mediante la API normal de BackupForge.

## Restricciones

- No uses red, reloj ni datos aleatorios.
- No sustituyas SHA-256 por metadata más débil.
- No ocultes errores de I/O.
- No uses `unwrap()`/`expect()` dentro de la biblioteca para este comportamiento recuperable.
- Conserva `cargo fmt`, Clippy y el resto de tests verdes.

## Evidencia esperada

Tu prueba debe demostrar al menos:

```text
reused = 0
copied = 1
verify_backup(...).is_valid() = true
```

Los nombres exactos pueden variar si tu diseño mantiene el mismo contrato.

## Reflexión

Responde brevemente:

- ¿Qué evidencia permite reutilizar un archivo?
- ¿Qué diferencia hay entre “el manifest dice que era correcto” y “la copia física sigue correcta”?
- ¿Qué limitación conserva este modelo incremental frente a snapshots históricos?
