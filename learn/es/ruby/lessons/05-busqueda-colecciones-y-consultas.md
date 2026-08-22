# Lección 05 — Búsqueda, colecciones y consultas

## Qué vas a conseguir

Buscarás contactos por nombre, email o empresa y filtrarás por estado sin convertir el controlador en una cadena de `if`.

## El problema

Un CRM deja de ser útil cuando sólo puede listar todo. Necesitamos encontrar a una persona con datos parciales y combinar filtros sin duplicar lógica.

## Concepto

Active Record devuelve relaciones: objetos que representan una consulta y pueden encadenarse antes de tocar la base de datos. `Contact.search(...).with_status(...)` conserva esa composición.

Ruby aporta aquí métodos de clase, retornos tempranos y strings. Rails aporta la traducción de la relación a SQL.

## Código real

- Consulta: [`../app/app/models/contact.rb`](../app/app/models/contact.rb)
- Composición HTTP: [`../app/app/controllers/contacts_controller.rb`](../app/app/controllers/contacts_controller.rb)
- Formulario GET: [`../app/app/views/contacts/index.html.erb`](../app/app/views/contacts/index.html.erb)

[DEMO] Busca `norte`, cambia el estado a `Cliente` y observa cómo la URL conserva los parámetros.

## Tu turno

Añade tres contactos y prueba búsquedas con mayúsculas, fragmentos de email y empresa. Después ejecuta `Contact.search("norte").to_sql` en `bin/rails console` para observar la consulta generada.

## Cómo comprobar tu solución

```bash
bin/rails test test/models/contact_test.rb
bin/rails test test/integration/contacts_flow_test.rb
```

## Errores comunes

- Construir SQL concatenando texto del usuario.
- Cargar todos los registros con `Contact.all.to_a` y filtrar después en Ruby.
- Hacer que un filtro vacío elimine todos los resultados.

## Siguiente paso

Continúa con [Lección 06 — editar sin duplicar formularios](06-editar-sin-duplicar-formularios.md).

## Referencias

- https://guides.rubyonrails.org/active_record_querying.html
- https://api.rubyonrails.org/classes/ActiveRecord/Sanitization/ClassMethods.html
