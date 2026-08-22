# Ruby desde cero — ContactDesk

Aprenderás Ruby construyendo **ContactDesk**, un CRM web local para registrar clientes y prospectos. El curso usa Ruby 4.0 y Rails 8.1 como aplicación canónica, pero cada lección explica el lenguaje detrás del framework en lugar de tratar Rails como magia.

## Qué necesitas

- Ruby 4.0.6.
- Bundler.
- SQLite.
- Un navegador y una terminal.

Git se aprende en el [curso transversal de Git](../git/); aquí sólo lo enlazamos cuando haga falta control de versiones.

## Ejecutar ContactDesk

Desde `learn/es/ruby/app`:

```bash
bundle install
bin/rails db:prepare
bin/rails test
bin/rails server
```

Abre `http://localhost:3000`.

## Lecciones

1. [Ruby ejecutable y una aplicación web real](lessons/01-ruby-ejecutable-y-una-app-real.md)
2. [Objetos, atributos y un contacto válido](lessons/02-objetos-atributos-y-contactos.md)
3. [Métodos, condiciones y validaciones](lessons/03-metodos-condiciones-y-validaciones.md)
4. [HTTP, controladores y persistencia](lessons/04-http-controladores-y-persistencia.md)
5. [Búsqueda, colecciones y consultas](lessons/05-busqueda-colecciones-y-consultas.md)
6. [Editar sin duplicar formularios](lessons/06-editar-sin-duplicar-formularios.md)
7. [Asociaciones y notas de seguimiento](lessons/07-asociaciones-y-notas.md)
8. [Encadenar consultas y probar comportamiento](lessons/08-encadenar-consultas-y-probar.md)

### Checkpoints

- [Checkpoint 01 — extiende la ficha del contacto](exercises/checkpoint-01.md)
- [Checkpoint 02 — seguimiento de contactos](exercises/checkpoint-02.md)

## Estado

Segundo incremento: **8/17 lecciones**. ContactDesk ya permite buscar y filtrar, editar contactos y registrar notas asociadas con pruebas de modelo e integración.

## Qué aprenderás al completar el curso

Ruby idiomático, colecciones, métodos, clases y módulos, errores, archivos y persistencia, HTTP, MVC en Rails, Active Record, pruebas, debugging, dependencias, seguridad básica web y mantenimiento de una aplicación existente.

## Referencias oficiales

- [Ruby Documentation](https://www.ruby-lang.org/en/documentation/)
- [Ruby on Rails Guides](https://guides.rubyonrails.org/)
- [Rails API](https://api.rubyonrails.org/)

## Empleabilidad

Ruby se usa profesionalmente sobre todo en aplicaciones web y productos construidos con Rails. Este curso busca una base Junior razonable; no promete empleo.
