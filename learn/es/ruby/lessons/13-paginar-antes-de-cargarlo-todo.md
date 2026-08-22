# Lección 13 — Paginar antes de cargarlo todo

## Qué vas a conseguir

Harás que ContactDesk mantenga tiempos y memoria razonables cuando la lista crece, sin añadir una gema sólo para paginar.

## El problema

`Contact.all` funciona con pocos registros, pero una aplicación real no debe asumir que siempre habrá diez contactos.

## Concepto

Rails permite paginar con `limit` y `offset`. La aplicación fija `PAGE_SIZE = 20`, cuenta el scope filtrado y conserva búsqueda/estado al navegar. El límite es parte del contrato: el navegador no decide arbitrariamente cuántas filas cargar.

## Código real

- [`../app/app/controllers/contacts_controller.rb`](../app/app/controllers/contacts_controller.rb)
- [`../app/app/views/contacts/index.html.erb`](../app/app/views/contacts/index.html.erb)

[EJECUTAR]

```bash
bin/rails test test/integration/contacts_flow_test.rb
```

## Tu turno

Explica por qué primero componemos búsqueda/filtro y después aplicamos `offset/limit`. ¿Qué resultado incorrecto obtendrías si paginaras antes de filtrar?

## Buenas prácticas

- límites explícitos;
- orden estable antes de paginar;
- no cargar toda la tabla para cortar un arreglo en Ruby;
- medir antes de añadir cache o infraestructura.

## Siguiente paso

Continúa con [Lección 14 — Tareas operativas sin tocar datos](14-tareas-operativas-sin-tocar-datos.md).

## Referencias

- https://guides.rubyonrails.org/active_record_querying.html#limit-and-offset
