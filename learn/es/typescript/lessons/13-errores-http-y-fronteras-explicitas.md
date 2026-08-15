# Lección 13 — Errores HTTP y fronteras explícitas

## Qué vas a conseguir

Convertirás fallas de la frontera HTTP en contratos precisos sin contaminar el dominio con códigos de transporte.

## Antes de empezar

Completa la [Lección 12](12-contratos-de-error-y-checkpoint-03.md) y ejecuta `npm run verify`.

## El problema

FreelanceDesk ya distingue una persistencia fallida, pero cualquier otro error capturado termina en `400`. Además, el lector de JSON no exige que el consumidor declare qué formato está enviando. Una API pequeña puede funcionar así durante una demo y aun ser ambigua para un cliente real.

## Concepto

El tipo no reemplaza la validación de una frontera externa. HTTP entrega strings, headers y bytes. Por eso la capa server puede usar un error operativo con `statusCode`, mientras el dominio conserva reglas independientes de HTTP.

## Demostración

[DEMO] Revisa `HttpFailure`, `PersistenceFailure` y `requireJsonContentType` en `app/src/server/app.ts`. Observa que una petición `text/plain` obtiene `415`, mientras una regla de negocio inválida continúa en `400`.

## Código real

`readJson` valida `Content-Type`, tamaño y presencia de cuerpo antes de entregar un valor al código de aplicación. La frontera decide el status; `createProject` y `changeQuoteStatus` siguen sin saber qué es HTTP.

## Qué acaba de pasar

La API ahora puede expresar “no entiendo este formato” de forma distinta a “entiendo tu JSON, pero viola una regla”. Esa diferencia es observable y testeable.

## Errores comunes

- Creer que una interfaz TypeScript valida JSON recibido por red.
- Exponer errores internos completos al cliente.
- Colocar códigos HTTP dentro del dominio.
- Crear decenas de clases de error cuando dos o tres contratos explícitos bastan.

## Buenas prácticas

Valida en la frontera y conserva el núcleo independiente. Un error debe ayudar al consumidor a decidir si corrige datos, cambia el formato o reintenta.

## Tu turno

[PAUSA PARA EJERCICIO] Envía una creación de cliente con `text/plain`, luego con JSON malformado y explica por qué ambos fallan por razones distintas aunque ninguno deba persistirse.

## Cómo comprobar

```bash
npm run verify
```

La regresión operativa debe demostrar `415` sin escrituras al store.

## Solución enlazada

La implementación canónica está en `app/src/server/app.ts`; primero intenta explicar la frontera sin copiarla.

## Reto adicional

Diseña cómo representarías `409 Conflict` si más adelante existiera control de versiones optimista. No lo implementes sin una necesidad real.

## Resumen

Los tipos internos y los contratos HTTP resuelven problemas distintos. Una frontera profesional hace explícita esa separación.

## Siguiente paso

Continúa con la [Lección 14 — Tooling y gate profesional](14-tooling-y-gate-profesional.md).

## Referencias

- [HTTP 415 — MDN](https://developer.mozilla.org/docs/Web/HTTP/Status/415)
- [HTTP messages — MDN](https://developer.mozilla.org/docs/Web/HTTP/Messages)
- [TypeScript — Narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html)
