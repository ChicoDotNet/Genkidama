# Lección 11 — Mutaciones durables y fallas asíncronas

## Qué vas a conseguir

Harás que una mutación sólo se vuelva visible en memoria después de que la persistencia confirme el cambio.

## Antes de empezar

Completa la [Lección 10](10-ciclo-comercial-de-cotizaciones.md).

## El problema

La versión anterior hacía `state.projects.push(...)` y después `await store.save(...)`. Si el disco fallaba, el cliente recibía error pero `GET /api/projects` mostraba un proyecto que nunca quedó guardado. Reiniciar el proceso lo hacía desaparecer.

## Concepto

Una operación durable necesita un orden observable coherente. En FreelanceDesk construimos un snapshot candidato, intentamos persistirlo y sólo entonces reemplazamos el estado en memoria. No es una transacción de base de datos; sí es una garantía local útil: **si `save` falla, el estado anterior sigue siendo la verdad del proceso**.

## Demostración

[DEMO] `CaptureStore` puede fallar en el siguiente `save`. La regresión crea un cliente, fuerza una falla al crear un proyecto y comprueba tres cosas:

1. HTTP responde `503`;
2. el mensaje no expone detalles internos del disco;
3. una consulta posterior sigue devolviendo cero proyectos.

[EJECUTAR]

```bash
npm test
```

## Código real

`commitSnapshot` espera a `store.save(next)` antes de reemplazar `clients`, `quotes` y `projects`. Si la frontera de persistencia falla, traduce el detalle técnico a `PersistenceFailure`.

## Qué acaba de pasar

Una falla asíncrona ya no deja memoria y disco contando historias distintas.

## Errores comunes

- Mutar primero y asumir que un `catch` revierte automáticamente.
- Reintentar una escritura no idempotente sin conocer sus efectos.
- Devolver al usuario la ruta del archivo o el error crudo del sistema operativo.
- Llamar “transacción” a una garantía que sólo existe dentro de un proceso.

## Buenas prácticas

Define qué significa éxito antes de mutar estado observable. Conserva fallas técnicas en la frontera y tradúcelas a un contrato operativo útil.

## Tu turno

Protege con el mismo patrón un cambio de estado de cotización. Simula una falla de `save` y confirma que la cotización conserva su estado anterior.

## Cómo comprobar

```bash
npm run verify
```

La suite debe demostrar explícitamente el caso de persistencia fallida.

## Solución enlazada

Compara con `commitSnapshot` después de tu intento.

## Reto adicional

Explica qué cambia si dos procesos escriben el mismo JSON al mismo tiempo. Identifica la limitación sin intentar convertir este archivo en una base de datos multiusuario.

## Resumen

`await` no da atomicidad por sí solo. El orden de efectos y la prueba del camino de falla sí pueden preservar un contrato local consistente.

## Siguiente paso

Continúa con la [Lección 12](12-contratos-de-error-y-checkpoint-03.md).

## Referencias

- [async function — MDN](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Statements/async_function)
- [Node.js File system promises](https://nodejs.org/api/fs.html#promises-api)
