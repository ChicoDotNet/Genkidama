# Lección 07 — Asociaciones y notas de seguimiento

## Qué vas a conseguir

Registrarás notas cronológicas asociadas a cada contacto para convertir ContactDesk en una herramienta de seguimiento, no sólo una libreta de direcciones.

## El problema

Una nota pertenece a un contacto y un contacto puede tener muchas notas. Guardar todo en una sola columna de texto impediría validar, ordenar y evolucionar cada interacción.

## Concepto

`has_many` y `belongs_to` expresan una relación uno-a-muchos. La clave foránea en SQLite refuerza que una nota no quede huérfana.

Las rutas anidadas sólo se usan para crear una nota dentro de su contacto: `/contacts/:contact_id/notes`. El recurso contacto conserva su propia página `show`.

## Código real

- Modelos: [`../app/app/models/contact.rb`](../app/app/models/contact.rb) y [`../app/app/models/note.rb`](../app/app/models/note.rb)
- Migración: [`../app/db/migrate/20260822000200_create_notes.rb`](../app/db/migrate/20260822000200_create_notes.rb)
- Controlador: [`../app/app/controllers/notes_controller.rb`](../app/app/controllers/notes_controller.rb)
- Vista: [`../app/app/views/contacts/show.html.erb`](../app/app/views/contacts/show.html.erb)

## Tu turno

Agrega dos notas a un contacto. Intenta enviar una vacía y comprueba que recibes `422` con un error visible.

## Cómo comprobar tu solución

```bash
bin/rails db:prepare
bin/rails test test/models/note_test.rb
bin/rails test test/integration/contacts_flow_test.rb
```

## Errores comunes

- Guardar `contact_id` recibido sin verificar que el contacto exista.
- Permitir notas vacías.
- Borrar un contacto y dejar registros hijos sin una política explícita.

## Siguiente paso

Continúa con [Lección 08 — encadenar consultas y probar comportamiento](08-encadenar-consultas-y-probar.md).

## Referencias

- https://guides.rubyonrails.org/association_basics.html
- https://guides.rubyonrails.org/routing.html#nested-resources
