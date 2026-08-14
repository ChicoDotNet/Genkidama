# Lección 12 — Contratos de error y Checkpoint 03

## Qué vas a conseguir

Distinguirás errores de entrada de fallas operativas y cerrarás el bloque con un checkpoint que exige conservar estado ante una persistencia fallida.

## Antes de empezar

Completa la [Lección 11](11-mutaciones-durables-y-fallas-asincronas.md).

## El problema

Responder `400` a todo mezcla dos situaciones muy distintas: “tu petición es inválida” y “el servidor no pudo guardar una petición válida”. Para quien consume una API, esa diferencia cambia la decisión de corregir datos o reintentar después.

## Concepto

FreelanceDesk usa un contrato sencillo:

- `400`: entrada, entidad, filtro o transición inválida;
- `404`: ruta inexistente;
- `503`: la operación era válida pero la persistencia no pudo confirmar el cambio.

No es una taxonomía universal. Es un contrato local explícito, testeable y suficiente para esta app.

## Demostración

[EN PANTALLA] Compara una transición inválida de proyecto con una falla simulada de `AppStateStore.save`. Ambas terminan en `catch`, pero no significan lo mismo ni producen el mismo status.

## Código real

`PersistenceFailure` permanece en la capa HTTP/operación. El dominio sigue expresando errores de reglas mediante excepciones claras y no conoce códigos HTTP.

## Qué acaba de pasar

El cliente puede distinguir “corrige la petición” de “la infraestructura no confirmó el cambio”, y el proceso conserva estado coherente en ambos caminos.

## Errores comunes

- Exponer stack traces o rutas locales en la respuesta.
- Convertir fallas de infraestructura en 400 para simplificar el handler.
- Introducir una jerarquía enorme de errores sin necesidad real.
- Reintentar automáticamente una mutación sin analizar idempotencia.

## Buenas prácticas

Haz explícitos los errores que cambian el comportamiento del consumidor. Conserva detalles diagnósticos sensibles fuera del contrato público.

## Tu turno — Checkpoint 03

Resuelve [`../exercises/checkpoint-03.md`](../exercises/checkpoint-03.md) sin abrir la solución. Extiende el ciclo de cotizaciones y demuestra que una falla de persistencia al enviar una cotización no cambia su estado en memoria.

## Cómo comprobar

```bash
npm run verify
```

Debes tener evidencia de filtros, transiciones comerciales, compatibilidad de JSON legacy y rollback lógico ante `save()` fallido.

## Solución enlazada

Después de tu intento consulta [`../solutions/checkpoint-03.md`](../solutions/checkpoint-03.md).

## Reto adicional

Diseña un pequeño `Result` discriminado para errores esperados y compáralo con excepciones. No lo adoptes sólo por moda: explica qué llamadas ganarían claridad.

## Resumen

Un sistema confiable no sólo modela el camino feliz; distingue fallas, conserva invariantes y da al consumidor información suficiente para actuar.

## Siguiente paso

Continúa con la [Lección 13 — Errores HTTP y fronteras explícitas](13-errores-http-y-fronteras-explicitas.md).

## Referencias

- [HTTP response status codes — MDN](https://developer.mozilla.org/docs/Web/HTTP/Status)
- [TypeScript — Narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html)
