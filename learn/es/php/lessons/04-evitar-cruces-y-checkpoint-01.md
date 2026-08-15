# Lección 04 — Evitar cruces de horario + Checkpoint 01

## Qué vas a conseguir

Vas a integrar la primera regla de agenda que cruza varias citas: impedir solapamientos, presentar el error y conservar la entrada del usuario.

## Antes de empezar

Completa la [Lección 03](03-formulario-post-y-persistencia.md).

## El problema

Una agenda que permite reservar dos citas sobre el mismo recurso a la misma hora produce un dato válido en formato pero inválido para el negocio.

## Concepto

[`Schedule`](../app/src/Domain/Schedule.php) posee la colección y la regla que necesita comparar una nueva cita con las existentes. Antes de agregar revisa ID duplicado y traslape temporal.

La interfaz web no intenta repetir esa regla. Sólo captura `DomainException`, responde 422 y muestra el mensaje.

## Demostración

[DEMO] Registra una cita 10:00–11:00. Luego intenta 10:30–11:00. Debes ver un error textual y conservar los valores enviados. Después registra una a las 11:00 exactas: debe aceptarse.

## Código real

```text
POST formulario
  → AppointmentService::book
  → Appointment
  → Schedule::add
  → AppointmentStore::save
  → redirect 303
```

Si `Schedule::add` falla, `save()` no se ejecuta. La UI usa controles nativos, labels, foco visible, `role="alert"` y `aria-describedby` durante un error.

## Qué acaba de pasar

La primera regla multi-entidad quedó en el dominio, no en la plantilla ni el store. La misma regla protegería una API o CLI futura.

## Errores comunes

- Detectar conflictos sólo con JavaScript del navegador.
- Considerar 11:00 inicio como conflicto con una cita que termina exactamente a las 11:00.
- Borrar lo que el usuario escribió después de un error.
- Persistir primero y validar después.

## Buenas prácticas

Los errores conocidos deben ayudar a recuperarse. La interfaz conserva la entrada, mantiene la acción primaria visible y distingue mensaje de éxito/error por texto además de estilo.

## Tu turno — Checkpoint 01

[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-01.md`](../exercises/checkpoint-01.md) sin abrir la solución.

## Cómo comprobar

```bash
bash tools/verify.sh
bash tools/smoke.sh
```

Además prueba manualmente una cita adyacente y una que se cruza.

## Solución enlazada

Consulta [`../solutions/checkpoint-01.md`](../solutions/checkpoint-01.md) sólo después de completar un intento.

## Reto adicional

Piensa dónde viviría una regla “un cliente no puede tener más de tres citas futuras”. ¿Es regla de `Appointment`, `Schedule`, persistencia o UI? Justifica tu respuesta.

## Resumen

- Las reglas que comparan citas pertenecen al calendario.
- Validar antes de persistir evita estado inválido.
- Los límites exactos del intervalo importan.
- Una recuperación usable conserva entrada y explica qué ocurrió.

## Siguiente paso

Continúa con [Lección 05 — Encontrar y cancelar citas](05-encontrar-y-cancelar-citas.md).

## Referencias

- [DomainException — PHP manual](https://www.php.net/manual/en/class.domainexception.php)
- [DateTimeImmutable — PHP manual](https://www.php.net/manual/en/class.datetimeimmutable.php)
- [WCAG 2.2 — Error Identification](https://www.w3.org/WAI/WCAG22/Understanding/error-identification.html)
