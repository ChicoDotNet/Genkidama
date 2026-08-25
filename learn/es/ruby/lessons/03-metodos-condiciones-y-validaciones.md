# Lección 03 — Métodos, condiciones y validaciones

## Qué vas a conseguir

Leerás métodos Ruby y condiciones dentro de un flujo que decide si un contacto puede guardarse.

## El problema

La aplicación debe responder distinto ante datos válidos e inválidos sin esconder el error.

## Concepto

Ruby usa métodos pequeños y expresivos. Rails aprovecha esa sintaxis en métodos como `save`, que devuelve un valor booleano utilizable directamente en un `if`.

Ver [`../app/app/controllers/contacts_controller.rb`](../app/app/controllers/contacts_controller.rb):

```ruby
if @contact.save
  redirect_to contacts_path, notice: "Contacto creado."
else
  render :new, status: :unprocessable_entity
end
```

La rama de error conserva el objeto y sus errores; no finge éxito ni descarta silenciosamente la causa.

## Tu turno

Cambia temporalmente un test para enviar un estado `unknown`. Predice qué rama ejecutará el controlador y después verifica tu predicción.

## Cómo comprobar tu solución

```bash
bin/rails test
```

Debe mantenerse el contrato: datos inválidos no incrementan `Contact.count`.

## Errores comunes

- Usar excepciones para una validación esperable del usuario.
- Convertir cualquier error en `200 OK`.
- Añadir lógica de negocio a la vista sólo porque ahí resulta visible.

## Siguiente paso

[Lección 04 — HTTP, controladores y persistencia](04-http-controladores-y-persistencia.md)

## Referencias

- https://docs.ruby-lang.org/en/master/syntax/control_expressions_rdoc.html
- https://guides.rubyonrails.org/action_controller_overview.html
