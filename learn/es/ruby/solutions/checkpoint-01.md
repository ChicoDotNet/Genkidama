# Solución de referencia — Checkpoint 01

Una solución pequeña mantiene el cambio en cuatro fronteras: esquema, parámetros permitidos, presentación y prueba.

## Migración

```ruby
class AddPhoneToContacts < ActiveRecord::Migration[8.1]
  def change
    add_column :contacts, :phone, :string
  end
end
```

## Controlador

Añade `:phone` a la lista de `permit` de `contact_params`.

## Vista

Añade un `form.telephone_field :phone` y muestra `contact.phone` en la tabla. El campo permanece opcional; no hay razón de negocio para inventar una validación obligatoria.

## Prueba

Extiende el flujo de creación enviando `phone: "5551234567"` y verifica el valor persistido.

## Por qué esta solución

No crea una segunda representación del teléfono ni mete reglas de formato prematuras. El checkpoint busca practicar migraciones, atributos, parámetros y pruebas; una política internacional de teléfonos requiere requisitos propios y puede añadirse cuando exista una necesidad real.
