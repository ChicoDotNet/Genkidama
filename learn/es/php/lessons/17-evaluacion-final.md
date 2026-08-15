# Lección 17 — Evaluación final sin receta

## Qué vas a conseguir

Demostrarás que puedes leer, modificar, probar y explicar AgendaPHP sin seguir una receta paso a paso. Esta evaluación integra dominio, persistencia, HTTP, debugging, medición, seguridad básica y tooling de PHP.

## Antes de empezar

Completa la [Lección 16](16-hardening-http-y-checkpoint-04.md). Desde `app/` ejecuta:

```bash
bash tools/verify.sh
bash tools/smoke.sh
```

Parte de una rama o copia limpia de tu trabajo. Si necesitas repasar control de versiones, usa el [curso transversal de Git](../../git/).

## El problema

Un pequeño negocio ya usa AgendaPHP localmente. Quiere distinguir citas **confirmadas** de citas todavía **pendientes**, corregir una ambigüedad de entrada y conservar todos los contratos existentes: no cruces, identidad estable, persistencia consistente, filtros/CSV derivados de una sola fuente de verdad y rechazo temprano de requests no confiables.

No recibirás una lista de archivos o funciones que debas modificar.

## Concepto

Una evaluación profesional no mide si recuerdas una clase concreta. Mide si puedes **leer → formular una hipótesis → escribir una regresión → implementar → verificar → explicar** sobre una base existente.

Tu solución puede diferir de la referencia si conserva los contratos y puedes defender sus trade-offs.

## Demostración

[DEMO] Antes de escribir código, recorre `Domain`, `Application`, `Infrastructure`, `public/index.php` y las pruebas. Explica:

- dónde viven las reglas de una cita;
- dónde se decide si una mutación puede hacerse durable;
- qué piezas son representación HTTP/HTML/CSV;
- qué dependencia sería sospechosa si apuntara desde dominio hacia HTTP o almacenamiento.

## Código real

Abre [`../exercises/evaluacion-final.md`](../exercises/evaluacion-final.md) y resuelve las historias sobre la misma aplicación canónica. Puedes consultar las lecciones, mensajes de PHP/PHPUnit, `php.net`, Composer, PHPUnit y documentación oficial relevante.

No abras la solución antes de completar un intento serio.

## Qué acaba de pasar

Ya no estás ejecutando una receta. Estás manteniendo software existente: debes descubrir contratos, elegir dónde cambiar, proteger una regresión y explicar por qué tu solución no deteriora otras capacidades.

## Errores comunes

- Guardar el estado de confirmación sólo en HTML y perderlo al recargar.
- Duplicar colecciones para pendientes/confirmadas y crear dos fuentes de verdad.
- Cambiar persistencia sin conservar compatibilidad o sin validar datos rehidratados.
- Mutar el `Schedule` visible antes de confirmar `save`.
- Corregir una entrada ambigua sin prueba de regresión.
- Aceptar un nuevo POST sin CSRF/media-type/body-limit porque “sólo cambia un campo”.
- Confundir headers defensivos con una aplicación lista para Internet.
- Añadir un framework para resolver una capacidad que la arquitectura actual ya puede expresar claramente.

## Buenas prácticas

Mantén invariantes en dominio, coordinación en aplicación e I/O en fronteras. Prefiere cambios pequeños, tipos explícitos, pruebas observables y compatibilidad deliberada. Si una decisión aumenta complejidad, nombra qué problema real compra esa complejidad.

## Tu turno

[PAUSA PARA EJERCICIO] Completa las historias A–F de la evaluación. Después prepara una explicación de cinco minutos sobre arquitectura, persistencia, HTTP, seguridad acotada y el principal trade-off de tu solución.

## Cómo comprobar

Como mínimo:

```bash
bash tools/verify.sh
bash tools/smoke.sh
```

Además prueba manualmente:

1. crear una cita pendiente;
2. confirmarla y recargar la página;
3. filtrar/exportar sin perder el estado;
4. una entrada inválida;
5. una mutación sin CSRF;
6. una falla durable o fixture corrupto.

Usa la [`rúbrica final`](../exercises/rubrica-final.md) para autoevaluarte.

## Solución enlazada

Sólo después de tu intento, compara con [`../solutions/evaluacion-final.md`](../solutions/evaluacion-final.md). Es una dirección de referencia, no una exigencia de código idéntico.

## Reto adicional

Explica qué cambiaría si dos procesos o dos instancias escribieran la misma agenda. No implementes infraestructura distribuida: identifica la frontera que sustituirías, cómo evitarías lost updates y qué nueva observabilidad/seguridad necesitarías.

## Cómo hablar de este proyecto en una entrevista

Cuenta primero el problema: una agenda web local que preserva invariantes y estado durable. Explica después por qué las citas son intervalos semiabiertos, cómo mantienes una sola fuente de verdad, por qué `AppointmentStore` es una frontera, cómo distingues 422/503, qué protege CSRF, qué no protege, cómo una prueba de regresión guía un bugfix y qué señal justificaría migrar JSON a SQLite o adoptar un framework.

Preguntas probables:

- ¿Por qué `strict_types` no sustituye validación de dominio?
- ¿Dónde debe vivir el estado pendiente/confirmado y por qué?
- ¿Cómo conservarías compatibilidad con JSON creado por una versión anterior?
- ¿Por qué filtros, resumen y CSV deben derivarse del mismo `Schedule`?
- ¿Qué evita el patrón candidato → persistir → publicar?
- ¿Qué protege CSRF y qué queda fuera?
- ¿Cómo distinguirías un bug de dominio de una falla de infraestructura?
- ¿Cuándo dejaría de ser suficiente el archivo JSON?

## Resumen

Completar esta evaluación significa poder modificar una aplicación PHP real, proteger comportamiento con pruebas y explicar decisiones y límites. Es evidencia de preparación inicial Junior/Entry Level; no garantiza contratación.

## Siguiente paso

Repite los criterios débiles de la rúbrica y construye una variante propia de AgendaPHP sin copiar la solución de referencia.

## Referencias

- https://www.php.net/manual/en/
- https://www.php.net/manual/en/language.types.declarations.php
- https://www.php.net/manual/en/book.session.php
- https://docs.phpunit.de/
- https://getcomposer.org/doc/
