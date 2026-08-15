# Lección 08 — Ciclo de vida + Checkpoint 02

## Qué vas a conseguir

Vas a integrar crear, encontrar, editar y cancelar como un solo ciclo coherente, conservando identidad, validación de cruces y persistencia segura.

## Antes de empezar

Completa la [Lección 07](07-consultas-derivadas.md).

## El problema

Las operaciones aisladas pueden parecer correctas y aun así romperse al combinarse: una edición puede chocar, una cancelación puede apuntar al ID equivocado o un error puede dejar estado parcial.

## Concepto

AgendaPHP trata cada mutación como construcción de un **estado candidato**. Create agrega; update reemplaza conservando ID; cancel elimina por ID. Sólo después de validar se llama a `AppointmentStore::save()`.

## Demostración

[DEMO] Crea dos citas, edita una a un espacio libre, intenta una edición conflictiva y finalmente cancélala. La otra cita debe permanecer intacta en cada paso.

## Código real

El contrato se prueba en [`AppointmentServiceTest.php`](../app/tests/AppointmentServiceTest.php). La prueba de edición conflictiva captura el calendario previo y verifica que siga igual después del error.

La UI ofrece texto explícito para editar/cancelar, conserva foco visible y no convierte iconos o color en el único significado de una acción.

## Qué acaba de pasar

La aplicación ya tiene un ciclo de vida útil sin añadir framework ni base de datos innecesarios. El puerto de persistencia permite cambiar la implementación más adelante.

## Errores comunes

- Hacer delete/update directamente en la plantilla.
- Persistir antes de validar el candidato.
- Permitir que editar cambie identidad.
- Migrar a SQLite sin un problema concreto que compense schema/migraciones/transacciones.

## Buenas prácticas

Mantén reglas en dominio, orquestación en aplicación y filesystem/HTTP en los bordes. Una tecnología nueva debe pagar su complejidad resolviendo una necesidad visible.

## Tu turno — Checkpoint 02

[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-02.md`](../exercises/checkpoint-02.md) sin abrir la solución.

## Cómo comprobar

```bash
cd app
bash tools/verify.sh
bash tools/smoke.sh
```

Haz también el recorrido manual create → edit → intento conflictivo → cancel.

## Solución enlazada

Consulta [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md) sólo después de completar un intento.

## Reto adicional

Propón una migración futura a SQLite que mantenga `AppointmentStore` y explica qué pruebas actuales deberían pasar sin cambios.

## Resumen

- El ciclo completo conserva identidad e invariantes.
- Los estados candidatos protegen contra mutaciones parciales.
- JSON sigue siendo una decisión consciente y acotada.
- El puerto permite cambiar persistencia sin contaminar dominio.

## Siguiente paso

Continúa con la [Lección 09 — Consultas temporales sin duplicar estado](09-consultas-temporales.md).

## Referencias

- [Interfaces — PHP](https://www.php.net/manual/en/language.oop5.interfaces.php)
- [POST/Redirect/GET y 303 — HTTP Semantics](https://www.rfc-editor.org/rfc/rfc9110.html#name-303-see-other)
- [WCAG 2.2 — Focus Visible](https://www.w3.org/WAI/WCAG22/Understanding/focus-visible.html)
