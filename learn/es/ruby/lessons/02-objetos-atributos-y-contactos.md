# Lección 02 — Objetos, atributos y un contacto válido

## Qué vas a conseguir

Entenderás objetos, mensajes y atributos usando el modelo `Contact` que ya persiste en SQLite.

## El problema

Un CRM necesita distinguir datos válidos de datos incompletos antes de almacenarlos.

## Concepto

En Ruby casi todo es un objeto. Llamar `contact.valid?` envía el mensaje `valid?` al objeto. Rails añade Active Record para mapear ese objeto a una fila de base de datos.

[DEMO]

```ruby
contact = Contact.new(
  name: "Ana Torres",
  email: "ana@example.com",
  company: "Norte",
  status: "lead"
)

contact.valid?
contact.name
```

Observa los símbolos (`:name`), strings y el método predicado terminado en `?`.

## Código real

Ver [`../app/app/models/contact.rb`](../app/app/models/contact.rb).

El modelo exige nombre, email con formato razonable y un estado incluido en `lead`, `active` o `archived`.

## Tu turno

Abre `bin/rails console`, crea un contacto sin nombre y consulta `contact.errors.full_messages`.

## Cómo comprobar tu solución

Ejecuta:

```bash
bin/rails test test/models/contact_test.rb
```

## Buenas prácticas

Las validaciones del modelo protegen la regla de negocio independientemente de si el dato llegó desde una vista, consola o futura API.

## Siguiente paso

[Lección 03 — Métodos, condiciones y validaciones](03-metodos-condiciones-y-validaciones.md)

## Referencias

- https://docs.ruby-lang.org/en/master/syntax/methods_rdoc.html
- https://guides.rubyonrails.org/active_record_validations.html
