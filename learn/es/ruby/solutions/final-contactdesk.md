# Solución de referencia — evaluación final de ContactDesk

Esta referencia describe una solución proporcionada; no es la única válida. Compárala sólo después de intentar la evaluación.

## 1. Seguimiento vencido

Una opción pequeña es añadir `next_follow_up_on: date` a `contacts` y un scope que reciba explícitamente la fecha de referencia:

```ruby
scope :follow_up_due, ->(today = Date.current) {
  where.not(next_follow_up_on: nil).where(next_follow_up_on: ..today)
}
```

Pasar `today` desde el test permite probar la regla sin depender del reloj exacto del runner. Si el producto considera que “vence” sólo antes de hoy, usa `...today` y documenta esa regla.

## 2. Bugfix de búsqueda

La normalización debe tener una sola fuente. Una alternativa es exponer en `Contact` un método/clase pequeño que aplique `strip.downcase` al email y reutilizarlo tanto al persistir como al construir la condición de búsqueda. El controlador sólo entrega el término recibido; no debe conocer una segunda versión de la regla.

La regresión debe demostrar que buscar `"  PERSONA@EJEMPLO.COM  "` encuentra el contacto almacenado como `persona@ejemplo.com`.

## 3. Error CSV sin PII

Durante `CSV.foreach(...).with_index(2)` conserva el número lógico de fila. Cuando una fila falla, lanza `ContactTransfer::ImportError` con un mensaje como:

```text
La fila 4 no pudo importarse: datos inválidos.
```

No copies al error nombre, email, notas ni el contenido completo de la fila. Deja que la excepción ocurra dentro de la transacción para conservar rollback total.

Prueba dos cosas por separado:

- el mensaje identifica la fila;
- el conteo de contactos permanece igual después del fallo.

## 4. Filtro HTTP

Una implementación sencilla agrega un parámetro como `follow_up=due` a `ContactsController#index`, compone el scope antes de paginar y conserva ese parámetro en los enlaces de navegación.

La prueba de integración crea al menos un contacto vencido y otro futuro, solicita la URL con el filtro y verifica presencia/ausencia mediante datos de fixture controlados.

## 5. Diagnóstico sin PII

`Contactdesk::Diagnostics` puede agregar:

```ruby
follow_ups_due: Contact.follow_up_due.count
```

Eso mantiene el diagnóstico agregado. No necesitas añadirlo a `/healthz`: el health endpoint responde si la aplicación puede servir y alcanzar su dependencia esencial, no debe convertirse en inventario de negocio.

Añade una prueba que compruebe el conteo y que la representación diagnóstica no incluya emails conocidos de fixtures.

## 6. Recordatorios futuros

Una frontera mínima puede ser un objeto de aplicación como `FollowUpReminderCandidates` que seleccione IDs de contactos elegibles, o un servicio `FollowUpReminderPlanner` que produzca comandos/datos para otra frontera de entrega.

Dejaría fuera de alcance:

- proveedor SMTP/API;
- plantillas de correo;
- reintentos y jobs en background;
- tracking de entrega;
- preferencias/consentimiento del usuario.

Esas capacidades tienen requisitos propios. Preparar una frontera no justifica implementarlas antes de que exista una historia verificable.

## Pruebas y documentación

La solución debe conservar verdes:

```bash
bin/rails db:prepare
bin/rails test
RAILS_ENV=test bin/rails runner script/smoke.rb
bin/rails contactdesk:diagnostics
```

Para resolver dudas sobre scopes, tests, migraciones o seguridad, consulta primero Rails Guides/API. La documentación oficial debe respaldar decisiones; no sustituye las pruebas de tu propio contrato.

## Trade-off principal

Esta referencia prefiere cambios pequeños sobre la arquitectura existente. Un sistema de recordatorios completo podría justificar Active Job, adaptadores de entrega y políticas adicionales, pero introducirlos en esta evaluación ocultaría la competencia que se quiere medir: evolucionar ContactDesk con el mínimo diseño suficiente y evidencia ejecutable.
