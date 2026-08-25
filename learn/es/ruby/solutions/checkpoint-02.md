# Solución de referencia — Checkpoint 02

Una solución idiomática conserva la composición en Active Record:

```ruby
def self.without_notes
  left_outer_joins(:notes).where(notes: { id: nil })
end
```

Después el controlador puede aplicar la relación sólo cuando el parámetro existe:

```ruby
@contacts = Contact.search(params[:q]).with_status(params[:status])
@contacts = @contacts.without_notes if params[:without_notes] == "1"
@contacts = @contacts.order(:name)
```

En la vista, agrega un checkbox GET que conserve el valor de `without_notes`.

## Prueba mínima útil

Crea dos contactos; agrega una nota sólo a uno y verifica que `Contact.without_notes` devuelve exactamente el otro. Añade además una prueba HTTP que combine `without_notes=1` con un filtro ya existente.

## Por qué esta solución

La base de datos hace el filtro y el resultado sigue siendo una `ActiveRecord::Relation`, así que búsqueda, estado, orden y el nuevo criterio pueden componerse. No introduce una abstracción adicional ni un patrón por exhibición.
