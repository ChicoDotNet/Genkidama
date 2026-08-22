# Lección 15 — Observabilidad con health check y request ID

## Qué vas a conseguir

Expondrás una señal `/healthz` que un monitor puede consultar sin revelar PII y aprenderás por qué un request ID ayuda a seguir un fallo entre logs y respuestas.

## El problema

`200 OK` en la portada no demuestra que la base responda. Al mismo tiempo, un endpoint de salud no debe convertirse en un volcado de configuración o datos de clientes.

## Concepto

`Contactdesk::Diagnostics` produce un snapshot mínimo y reutilizable. `HealthController` traduce ese snapshot a HTTP y añade `request.request_id`, que Rails genera para correlacionar una petición con logs.

Si la base falla, el servicio degrada el estado y registra **la clase** del error, no emails ni secretos.

## Código real

- [`../app/app/controllers/health_controller.rb`](../app/app/controllers/health_controller.rb)
- [`../app/app/services/contactdesk/diagnostics.rb`](../app/app/services/contactdesk/diagnostics.rb)
- [`../app/test/integration/health_flow_test.rb`](../app/test/integration/health_flow_test.rb)

[EJECUTAR]

```bash
bin/rails test test/integration/health_flow_test.rb
bin/rails server
```

Luego consulta `/healthz`.

## Buenas prácticas

- no incluir nombres, emails, tokens ni stack traces en health;
- devolver código HTTP coherente con el estado;
- usar IDs de correlación;
- registrar errores accionables sin filtrar PII.

## Tu turno

Explica por qué el conteo total puede ser aceptable en este laboratorio mientras los contactos concretos no lo son. En un producto real, ¿qué clasificación de datos revisarías antes de exponer incluso ese conteo?

## Siguiente paso

Cerraremos el bloque separando lo que significa “listo para desplegar” de “funciona en mi máquina”.

## Referencias

- https://guides.rubyonrails.org/debugging_rails_applications.html#the-logger
- https://api.rubyonrails.org/classes/ActionDispatch/RequestId.html
