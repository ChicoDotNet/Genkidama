# Lección 04 — HTTP, controladores y persistencia

## Qué vas a conseguir

Conectarás una petición web con rutas, controlador, modelo, SQLite y una respuesta observable.

## El problema

Guardar un prospecto no es sólo “insertar una fila”: la aplicación debe aceptar parámetros permitidos, validar, persistir y responder correctamente.

## Concepto

Rails organiza el flujo MVC:

```text
Navegador -> ruta -> ContactsController -> Contact -> SQLite
                         |
                         +-> vista / redirect
```

`contact_params` limita los atributos aceptados desde HTTP. Esta frontera evita asignar cualquier campo sólo porque alguien lo envió en la petición.

## Código real

- Rutas: [`../app/config/routes.rb`](../app/config/routes.rb)
- Controlador: [`../app/app/controllers/contacts_controller.rb`](../app/app/controllers/contacts_controller.rb)
- Migración: [`../app/db/migrate/20260822000100_create_contacts.rb`](../app/db/migrate/20260822000100_create_contacts.rb)
- Flujo HTTP probado: [`../app/test/integration/contacts_flow_test.rb`](../app/test/integration/contacts_flow_test.rb)

## Tu turno

Crea un prospecto desde el navegador. Después abre `bin/rails console` y localízalo con `Contact.find_by(email: "...")`.

## Cómo comprobar tu solución

```bash
bin/rails db:prepare
bin/rails test
bin/rails runner 'puts Contact.count'
```

## Checkpoint

Antes de seguir, completa [Checkpoint 01 — extiende la ficha del contacto](../exercises/checkpoint-01.md).

## Siguiente paso

El siguiente incremento añadirá búsqueda, actualización, notas y pruebas más profundas manteniendo ContactDesk como única aplicación.

## Referencias

- https://guides.rubyonrails.org/routing.html
- https://guides.rubyonrails.org/active_record_basics.html
