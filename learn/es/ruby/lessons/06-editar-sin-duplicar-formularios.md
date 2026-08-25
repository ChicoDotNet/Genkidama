# Lección 06 — Editar sin duplicar formularios

## Qué vas a conseguir

Actualizarás contactos existentes reutilizando el mismo formulario de alta y conservando las validaciones del modelo.

## El problema

Copiar el formulario para editar crea dos lugares que se desincronizan. Además, un `UPDATE` debe validar igual que un `INSERT`.

## Concepto

`form_with model:` inspecciona si el objeto es nuevo o persistido y elige la ruta/verbo adecuados. Un partial `_form.html.erb` permite reutilizar presentación sin inventar una abstracción mayor.

El controlador carga el contacto una vez con `before_action` y `update` devuelve `422` cuando las reglas de dominio no se cumplen.

## Código real

- Controlador: [`../app/app/controllers/contacts_controller.rb`](../app/app/controllers/contacts_controller.rb)
- Partial: [`../app/app/views/contacts/_form.html.erb`](../app/app/views/contacts/_form.html.erb)
- Edición: [`../app/app/views/contacts/edit.html.erb`](../app/app/views/contacts/edit.html.erb)

## Tu turno

Convierte un prospecto a cliente y cambia su empresa. Intenta después dejar el nombre vacío y confirma que la edición no persiste.

## Cómo comprobar tu solución

```bash
bin/rails test test/integration/contacts_flow_test.rb
```

## Buenas prácticas

La reutilización aquí responde a una duplicación real. No necesitas una jerarquía de clases ni un patrón de diseño para compartir cuatro campos.

## Siguiente paso

Continúa con [Lección 07 — asociaciones y notas de seguimiento](07-asociaciones-y-notas.md).

## Referencias

- https://guides.rubyonrails.org/form_helpers.html
- https://guides.rubyonrails.org/action_controller_overview.html
