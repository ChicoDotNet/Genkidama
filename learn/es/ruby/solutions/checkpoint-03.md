# Solución de referencia — Checkpoint 03

La implementación de referencia concentra el contrato CSV en `ContactTransfer` y deja al controlador únicamente la traducción HTTP.

Las decisiones importantes son:

- `MAX_BYTES` limita memoria y superficie de entrada antes de parsear;
- `HEADERS` define una sola lista canónica de columnas;
- `CSV.parse(..., headers: true)` produce filas nombradas;
- el email se normaliza antes de `find_or_initialize_by`;
- `Contact.transaction` vuelve atómica la unidad de trabajo;
- `contact.save` reutiliza las validaciones del dominio;
- una fila inválida levanta `ContactTransfer::ImportError`, provocando rollback;
- el controlador rescata sólo `ImportError` y no oculta defectos inesperados.

Ver implementación:

- [`../app/app/services/contact_transfer.rb`](../app/app/services/contact_transfer.rb)
- [`../app/app/controllers/contacts_controller.rb`](../app/app/controllers/contacts_controller.rb)
- [`../app/test/services/contact_transfer_test.rb`](../app/test/services/contact_transfer_test.rb)

Una alternativa válida sería procesar archivos grandes por streaming. No se eligió todavía porque el límite de 256 KiB mantiene pequeño y verificable el problema de este curso; introducir streaming ahora agregaría complejidad sin una necesidad demostrada.
