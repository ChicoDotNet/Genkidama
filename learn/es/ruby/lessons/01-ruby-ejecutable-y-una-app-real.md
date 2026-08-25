# Lección 01 — Ruby ejecutable y una aplicación web real

## Qué vas a conseguir

Vas a comprobar el runtime, preparar ContactDesk y abrir una aplicación Rails real antes de estudiar sintaxis aislada.

## El problema

Un lenguaje se entiende mejor cuando puedes ejecutar algo y observar un resultado. ContactDesk será el mismo producto durante todo el curso.

## Concepto

Ruby es un lenguaje dinámico orientado a objetos. Rails es un framework web escrito en Ruby: aporta convenciones para HTTP, persistencia y vistas, pero las reglas que escribirás siguen siendo Ruby.

[EJECUTAR]

```bash
ruby --version
bundle install
bin/rails db:prepare
bin/rails server
```

Abre `http://localhost:3000`.

## Código real

Ver aplicación: [`../app/`](../app/).

`Gemfile` fija Ruby 4.0.6 y Rails 8.1.3.1 para que el curso sea reproducible. `config/routes.rb` conecta la raíz con la lista de contactos.

## Tu turno

Ejecuta `bin/rails routes` y localiza las rutas `contacts`. Explica con tus palabras qué operación HTTP usa cada una.

## Cómo comprobar tu solución

Debes poder abrir ContactDesk y ejecutar:

```bash
bin/rails test
```

sin errores.

## Errores comunes

- Ejecutar comandos fuera de `learn/es/ruby/app`.
- Confundir Ruby con Rails: Rails depende de Ruby, no lo sustituye.
- Instalar una versión preview aunque el curso fija una estable.

## Siguiente paso

[Lección 02 — Objetos, atributos y un contacto válido](02-objetos-atributos-y-contactos.md)

## Referencias

- https://www.ruby-lang.org/en/documentation/
- https://guides.rubyonrails.org/getting_started.html
