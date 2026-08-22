# Lección 08 — Encadenar consultas y probar comportamiento

## Qué vas a conseguir

Consolidarás búsqueda, filtros, actualización y notas con pruebas que observan comportamiento y no detalles internos de Rails.

## El problema

Una aplicación puede “verse bien” y aun romper reglas al cambiar una consulta o controlador. Necesitamos evidencia repetible sobre las capacidades que ya usa una persona.

## Concepto

Minitest permite probar dos niveles complementarios:

- modelo: reglas y consultas deterministas;
- integración: petición HTTP, persistencia y respuesta observable.

No comprobamos que Rails invoque métodos privados específicos. Comprobamos que buscar excluya registros no coincidentes, que editar persista y que una nota inválida produzca `422` con el formulario esperado.

## Código real

- Pruebas de modelo: [`../app/test/models/contact_test.rb`](../app/test/models/contact_test.rb)
- Pruebas de notas: [`../app/test/models/note_test.rb`](../app/test/models/note_test.rb)
- Flujo web: [`../app/test/integration/contacts_flow_test.rb`](../app/test/integration/contacts_flow_test.rb)

[PAUSA PARA EJERCICIO] Rompe temporalmente el filtro de estado, ejecuta la prueba y lee el mensaje antes de restaurarlo.

## Tu turno

Añade un caso que combine búsqueda por email con `status=active`. Haz que falle primero y corrige sólo lo necesario.

## Cómo comprobar tu solución

```bash
bin/rails test
```

## Checkpoint

Completa [Checkpoint 02 — seguimiento de contactos](../exercises/checkpoint-02.md) antes del siguiente bloque.

## Siguiente paso

Continúa con [Lección 09 — errores operativos y contratos explícitos](09-errores-operativos-y-contratos.md).

## Referencias

- https://guides.rubyonrails.org/testing.html
- https://guides.rubyonrails.org/active_record_querying.html
