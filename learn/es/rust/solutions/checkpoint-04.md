# Solución de referencia — Checkpoint 04

Una solución razonable conserva la semántica de `verify` y agrega una segunda comparación entre **conjunto esperado** y **conjunto observado**.

La implementación de referencia de este curso mantiene el audit en la frontera CLI:

1. carga el manifest validado;
2. ejecuta `verify_backup` para comprobar las entradas declaradas;
3. recorre el directorio en orden determinista;
4. excluye `manifest.json`;
5. compara rutas observadas con las rutas del manifest;
6. reporta por separado `mismatches` y `unexpected`.

La regresión mínima debe demostrar dos casos:

- un backup recién creado produce audit limpio;
- al inyectar `injected.txt`, el checksum de `data.txt` sigue correcto pero el audit reporta exactamente `injected.txt`.

No se borra el extra. Esa decisión pertenece al operador porque el programa no sabe si es residuo, evidencia o un archivo que debe incorporarse mediante un backup nuevo.

## Por qué no cambiamos `verify`

El comando existente ya tiene un contrato útil: “todo lo declarado conserva tamaño y SHA-256”. Cambiarlo para rechazar extras puede sorprender automatizaciones existentes. `audit` hace explícita la política más estricta.

## Límites

Esto no demuestra autenticidad del manifest frente a un atacante que pueda reemplazar manifest y archivos simultáneamente. Para esa amenaza necesitarías otra raíz de confianza: firma, almacenamiento inmutable, permisos/identidad, servicio remoto u otro control acorde al modelo de amenaza.
