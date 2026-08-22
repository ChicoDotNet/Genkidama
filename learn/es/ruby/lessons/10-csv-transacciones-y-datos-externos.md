# Lección 10 — CSV, transacciones y datos externos

## Qué vas a conseguir

Importarás y exportarás contactos mediante CSV sin permitir que una fila defectuosa deje media operación persistida.

## El problema

Los archivos externos no obedecen nuestras reglas. Una importación de 20 filas en la que la fila 14 es inválida no debe dejar 13 cambios aplicados y obligar a adivinar qué ocurrió.

## Concepto

`CSV.parse(..., headers: true)` convierte texto en filas direccionables por nombre de columna. Después cada fila vuelve a pasar por el modelo `Contact`: el importador no crea una segunda definición de “contacto válido”.

`Contact.transaction` hace la operación atómica. Si levantamos `ImportError` durante una fila, Active Record revierte los cambios hechos dentro de la transacción.

## Código real

[`../app/app/services/contact_transfer.rb`](../app/app/services/contact_transfer.rb)

El contrato tiene cuatro columnas canónicas:

```text
name,email,company,status
```

Los emails importados se normalizan antes de localizar un contacto existente. La exportación usa la biblioteca estándar `CSV`; no añadimos una gema para resolver una capacidad que Ruby ya incluye.

## Tu turno

Prepara un CSV con una fila válida y otra con email inválido. Comprueba con una prueba que la fila válida tampoco persiste.

## Cómo comprobar tu solución

```bash
bin/rails test test/services/contact_transfer_test.rb
```

## Buenas prácticas

- vuelve a validar datos importados;
- usa transacciones cuando la unidad de negocio sea “todo o nada”;
- no confíes en la extensión del archivo como validación;
- evita duplicar reglas del modelo en el parser.

## Siguiente paso

En la lección 11 pondremos límites de seguridad a la superficie HTTP que recibe esos archivos.

## Referencias

- https://ruby-doc.org/stdlib/libdoc/csv/rdoc/CSV.html
- https://guides.rubyonrails.org/active_record_transactions.html
