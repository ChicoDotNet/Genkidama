# Lección 12 — Debugging con evidencia, no con adivinanzas

## Qué vas a conseguir

Aprenderás a reducir un fallo de ContactDesk hasta una prueba reproducible y a distinguir si el problema está en HTTP, parsing, dominio o persistencia.

## El problema

Cuando una importación falla es tentador editar controlador, modelo y parser a la vez. Eso hace difícil saber qué arregló realmente el defecto y puede introducir regresiones.

## Concepto

Diagnosticar por capas reduce incertidumbre:

1. reproduce el dato mínimo que falla;
2. ejecuta primero la prueba más cercana al comportamiento;
3. identifica la frontera: request, CSV, modelo o base de datos;
4. corrige una causa demostrada;
5. ejecuta la suite completa y el smoke.

En este curso CI ya detectó diferencias reales entre shells y plataformas. La solución no fue desactivar Windows ni relajar pruebas: el smoke pasó a un archivo Ruby y usa el mismo `RAILS_ENV=test` que prepara el gate.

## Código real

- Pruebas del servicio: [`../app/test/services/contact_transfer_test.rb`](../app/test/services/contact_transfer_test.rb)
- Flujo HTTP: [`../app/test/integration/contacts_flow_test.rb`](../app/test/integration/contacts_flow_test.rb)
- Smoke portable: [`../app/script/smoke.rb`](../app/script/smoke.rb)

[EJECUTAR]

```bash
bin/rails test test/services/contact_transfer_test.rb
bin/rails test test/integration/contacts_flow_test.rb
bin/rails test
RAILS_ENV=test bin/rails runner script/smoke.rb
```

En PowerShell puedes establecer temporalmente `$env:RAILS_ENV = 'test'` antes del último comando.

## Tu turno

Rompe deliberadamente el encabezado `status` en el fixture de una prueba. Lee el error, explica qué frontera lo detectó y restaura la prueba.

## Checkpoint

Completa [Checkpoint 03 — intercambio seguro de contactos](../exercises/checkpoint-03.md).

## Siguiente paso

Continúa con [Lección 13 — Paginar antes de cargarlo todo](13-paginar-antes-de-cargarlo-todo.md).

## Referencias

- https://guides.rubyonrails.org/debugging_rails_applications.html
- https://guides.rubyonrails.org/testing.html
