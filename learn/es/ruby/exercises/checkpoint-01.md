# Checkpoint 01 — Extiende la ficha del contacto

## Objetivo

Añade un campo opcional `phone` a ContactDesk sin romper el flujo existente.

## Requisitos

1. Crea una migración que agregue `phone` a `contacts`.
2. Permite `phone` en `contact_params`.
3. Añade el campo al formulario y a la tabla de contactos.
4. Agrega al menos una prueba que demuestre que el teléfono se persiste.
5. Los contactos que no tengan teléfono deben seguir siendo válidos.

No cambies las validaciones de email/estado para hacer pasar el ejercicio.

## Evidencia

```bash
bin/rails db:migrate
bin/rails test
```

Después crea un contacto con teléfono desde el navegador y verifica el valor desde `bin/rails console`.

Cuando termines, compara tu enfoque con la [solución de referencia](../solutions/checkpoint-01.md).
