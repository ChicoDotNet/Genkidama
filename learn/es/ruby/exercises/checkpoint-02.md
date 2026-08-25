# Checkpoint 02 — seguimiento de contactos

Trabaja sobre ContactDesk sin copiar primero la solución.

## Objetivo

Añade una capacidad **“contactos sin seguimiento”**: una consulta que devuelva contactos que todavía no tienen notas y un filtro visible desde el listado.

## Criterios observables

- Escribe una prueba de modelo que cree un contacto con nota y otro sin nota.
- La consulta devuelve sólo el contacto sin notas.
- Añade un parámetro GET `without_notes=1` al listado.
- Combina correctamente con búsqueda y estado existentes.
- No cargues todos los contactos en memoria para filtrarlos en Ruby.
- `bin/rails test` queda verde.

## Pistas

Revisa `left_outer_joins`, `where` y la posibilidad de seguir devolviendo `ActiveRecord::Relation`.

Cuando tengas evidencia verde, compara tu solución con [la referencia](../solutions/checkpoint-02.md).
