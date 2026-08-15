# Lección 07 — Consultas derivadas sin segunda fuente de verdad

## Qué vas a conseguir

Vas a usar el calendario durable como única fuente de verdad y derivar orden, búsqueda por ID y vistas de edición sin persistir datos duplicados.

## Antes de empezar

Completa la [Lección 06](06-editar-sin-saltarse-las-reglas.md).

## El problema

Al crecer una aplicación es tentador guardar por separado “citas ordenadas”, “cita seleccionada” o “citas para editar”. Esa duplicación crea sincronización innecesaria.

## Concepto

`Schedule::all()` deriva orden cronológico y `find()` deriva selección por identidad desde la misma colección. La UI puede construir una vista sin convertirla en estado durable adicional.

## Demostración

[DEMO] Abre `/?edit=<id>`: AgendaPHP carga el mismo `Schedule`, localiza la cita y prellena el formulario. Cancelar edición vuelve a `/` sin mutar datos.

## Código real

Observa el flujo GET en [`public/index.php`](../app/public/index.php). `edit` es estado de presentación; la cita durable permanece en `AppointmentStore`.

## Qué acaba de pasar

Diferenciaste estado de negocio de estado temporal de UI. Menos duplicación significa menos invariantes accidentales.

## Errores comunes

- Persistir el “modo edición”.
- Mantener una segunda lista ordenada manualmente.
- Devolver una cita inexistente como `null` y ocultar el error.
- Usar el orden visual como identidad.

## Buenas prácticas

Deriva vistas cuando sean baratas y deterministas. Introduce índices, queries o una base de datos cuando una necesidad medida de volumen/consulta lo justifique.

## Tu turno

[PAUSA PARA EJERCICIO] Explica por qué JSON sigue siendo suficiente para este corte y escribe tres señales concretas que justificarían migrar el store a SQLite.

## Cómo comprobar

Abre una cita en edición, cancela la edición y confirma que el JSON no cambió.

## Solución enlazada

La [solución del Checkpoint 02](../solutions/checkpoint-02.md) incluye los criterios de persistencia usados en este bloque.

## Reto adicional

Diseña la firma de un futuro `appointmentsBetween($from, $to)` sin implementar SQL todavía.

## Resumen

- Las vistas derivadas no necesitan persistencia propia.
- El ID es identidad; el orden es una proyección.
- SQLite entra cuando resuelve una necesidad visible, no por exhibición.

## Siguiente paso

Continúa con [Lección 08 — Ciclo de vida y Checkpoint 02](08-ciclo-de-vida-y-checkpoint-02.md).

## Referencias

- [Arrays — PHP](https://www.php.net/manual/en/language.types.array.php)
- [usort — PHP](https://www.php.net/manual/en/function.usort.php)
