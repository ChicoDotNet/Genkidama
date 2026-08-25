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

## Aplicación final

ContactDesk permite registrar clientes/prospectos, buscar y filtrar contactos, editar fichas, guardar notas, importar/exportar CSV de forma acotada, paginar resultados y observar el estado operativo sin exponer PII. La aplicación mantiene errores explícitos, pruebas y una frontera clara entre health checks, diagnóstico y lógica de negocio.

## Lecciones

1. [Ruby ejecutable y una aplicación web real](lessons/01-ruby-ejecutable-y-una-app-real.md)
2. [Objetos, atributos y un contacto válido](lessons/02-objetos-atributos-y-contactos.md)
3. [Métodos, condiciones y validaciones](lessons/03-metodos-condiciones-y-validaciones.md)
4. [HTTP, controladores y persistencia](lessons/04-http-controladores-y-persistencia.md)
5. [Búsqueda, colecciones y consultas](lessons/05-busqueda-colecciones-y-consultas.md)
6. [Editar sin duplicar formularios](lessons/06-editar-sin-duplicar-formularios.md)
7. [Asociaciones y notas de seguimiento](lessons/07-asociaciones-y-notas.md)
8. [Encadenar consultas y probar comportamiento](lessons/08-encadenar-consultas-y-probar.md)
9. [Errores operativos y contratos explícitos](lessons/09-errores-operativos-y-contratos.md)
10. [CSV, transacciones y datos externos](lessons/10-csv-transacciones-y-datos-externos.md)
11. [Seguridad web y límites de entrada](lessons/11-seguridad-web-y-limites.md)
12. [Debugging con evidencia](lessons/12-debugging-con-evidencia.md)
13. [Paginar antes de cargarlo todo](lessons/13-paginar-antes-de-cargarlo-todo.md)
14. [Tareas operativas sin tocar datos](lessons/14-tareas-operativas-sin-tocar-datos.md)
15. [Observabilidad con health check y request ID](lessons/15-observabilidad-health-request-id.md)
16. [Hardening: listo para operar no significa invulnerable](lessons/16-hardening-y-entrega.md)
17. [Evaluación final: entrega ContactDesk](lessons/17-evaluacion-final.md)

### Checkpoints y evaluación

- [Checkpoint 01 — extiende la ficha del contacto](exercises/checkpoint-01.md)
- [Checkpoint 02 — seguimiento de contactos](exercises/checkpoint-02.md)
- [Checkpoint 03 — intercambio seguro de contactos](exercises/checkpoint-03.md)
- [Checkpoint 04 — prepara ContactDesk para operación](exercises/checkpoint-04.md)
- [Evaluación final — ContactDesk](exercises/final-contactdesk.md)

## Qué aprenderás al completar el curso

Ruby idiomático, colecciones, métodos, clases y módulos, errores, archivos y persistencia, HTTP, MVC en Rails, Active Record, pruebas, debugging, dependencias, seguridad básica web, observabilidad y mantenimiento de una aplicación existente.

## Pruebas y operación

Desde `app/`:

```bash
bin/rails db:prepare
bin/rails test
RAILS_ENV=test bin/rails runner script/smoke.rb
bin/rails contactdesk:diagnostics
```

El CI ejecuta la aplicación en Ubuntu y Windows. La base de desarrollo no es evidencia: las pruebas preparan su propio entorno y los diagnósticos evitan incluir nombres, emails o notas.

## Preguntas frecuentes

### ¿Puedo aprender Ruby sin saber Rails?

Sí. Rails es la aplicación real del curso, pero las lecciones explican objetos, métodos, bloques, colecciones, errores y módulos de Ruby cuando desbloquean una capacidad visible.

### ¿Por qué usar Rails en un curso desde cero?

Porque es una de las aplicaciones profesionales más relevantes de Ruby y permite practicar HTTP, base de datos, pruebas y mantenimiento sobre un producto coherente. El curso evita convertir generadores o convenciones en magia inexplicada.

### ¿Necesito una nube o servicios de pago?

No. ContactDesk funciona localmente con SQLite. Un despliegue real requeriría decisiones adicionales de hosting, HTTPS, secretos, backup, observabilidad y operación.

### ¿Este curso me garantiza trabajo?

No. El objetivo es una base Junior/Entry Level razonable sobre Ruby/Rails y evidencia que puedas explicar; la contratación depende de muchas otras capacidades y del mercado.

## Glosario

- **Ruby:** lenguaje dinámico orientado a objetos usado para scripting, automatización y aplicaciones.
- **Rails:** framework web de Ruby basado en convenciones y MVC.
- **Active Record:** capa de Rails que representa y consulta datos mediante modelos.
- **Migración:** cambio versionado del esquema de base de datos.
- **Scope:** consulta Active Record reutilizable y componible.
- **Request:** petición HTTP que entra a la aplicación.
- **CSRF:** ataque que intenta ejecutar una acción autenticada desde otro origen; Rails incluye protección para formularios mutables.
- **PII:** información que puede identificar a una persona; ContactDesk evita incluirla en diagnósticos operativos.
- **Smoke test:** comprobación corta de que la aplicación principal arranca y ejecuta un flujo esencial.

## Cómo hablar de ContactDesk en una entrevista

Explica primero el problema y después el framework. Un buen recorrido es: contactos y seguimiento → modelo/validaciones → consultas y asociaciones → CSV transaccional → límites de entrada → paginación → health/diagnósticos → pruebas multiplataforma. Señala un defecto real descubierto por CI y cómo una prueba o contrato evitó ocultarlo. La [Lección 17](lessons/17-evaluacion-final.md) incluye preguntas concretas para practicar.

## Referencias oficiales

- [Ruby Documentation](https://www.ruby-lang.org/en/documentation/)
- [Ruby on Rails Guides](https://guides.rubyonrails.org/)
- [Rails API](https://api.rubyonrails.org/)

## Siguiente paso

Después de completar la evaluación final, repite una historia sin consultar la solución, documenta el cambio y practica explicarlo con evidencia. Para control de versiones, ramas y colaboración usa el [curso transversal de Git](../git/).

## Empleabilidad

Ruby se usa profesionalmente sobre todo en aplicaciones web y productos construidos con Rails. Este curso busca una base Junior razonable; no promete empleo.
