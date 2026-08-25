# Checkpoint 03 — Intercambio seguro de contactos

Modifica ContactDesk para aceptar un CSV con contactos sin dejar datos parciales cuando una fila sea inválida.

## Requisitos

1. Conserva las columnas `name,email,company,status`.
2. Normaliza el email antes de buscar un contacto existente.
3. Rechaza archivos que excedan el límite configurado.
4. Si cualquier fila viola el modelo, revierte toda la importación.
5. Añade al menos una prueba que demuestre rollback y una que demuestre un error de contrato.
6. No rescates `StandardError` ni desactives CSRF para conseguir verde.

## Evidencia

Ejecuta:

```bash
bin/rails test test/services/contact_transfer_test.rb
bin/rails test
```

Después prueba manualmente una exportación y vuelve a abrir el CSV para inspeccionar sus encabezados.

Cuando termines, compara tu diseño con la [solución de referencia](../solutions/checkpoint-03.md).
