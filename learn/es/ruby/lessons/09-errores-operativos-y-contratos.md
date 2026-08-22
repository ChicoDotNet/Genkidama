# Lección 09 — Errores operativos y contratos explícitos

## Qué vas a conseguir

Separarás los errores que vienen de datos externos de los errores de programación y les darás un contrato entendible para la interfaz web.

## El problema

Un CSV puede estar incompleto, ser demasiado grande o contener una fila que viola las reglas de ContactDesk. Si dejamos escapar cualquier excepción, el usuario recibe un error 500 sin contexto; si rescatamos `StandardError`, ocultamos bugs reales.

## Concepto

Ruby permite definir excepciones específicas. `ContactTransfer::ImportError` significa: “la operación esperada de importación no pudo completarse por el archivo recibido”. No significa “algo desconocido falló”.

El controlador rescata sólo esa excepción y convierte el resultado en un mensaje útil. Un `NoMethodError`, un fallo de base de datos inesperado o un bug siguen siendo visibles para debugging.

## Código real

- Servicio: [`../app/app/services/contact_transfer.rb`](../app/app/services/contact_transfer.rb)
- Controlador: [`../app/app/controllers/contacts_controller.rb`](../app/app/controllers/contacts_controller.rb)

[DEMO] Intenta importar un CSV sin la columna `status` y observa el mensaje de negocio en vez de un stack trace en el navegador.

## Tu turno

Añade una prueba para un CSV sintácticamente roto. Debe terminar en `ContactTransfer::ImportError` y no crear contactos.

## Cómo comprobar tu solución

```bash
bin/rails test
```

## Errores comunes

- rescatar `StandardError` sólo para “que no truene”;
- devolver `false` sin explicar qué ocurrió;
- mostrar al usuario detalles internos de SQL o paths del servidor.

## Siguiente paso

Continúa con [Lección 10 — CSV, transacciones y datos externos](10-csv-transacciones-y-datos-externos.md).

## Referencias

- https://ruby-doc.org/3.4.1/Exception.html
- https://guides.rubyonrails.org/active_record_transactions.html
