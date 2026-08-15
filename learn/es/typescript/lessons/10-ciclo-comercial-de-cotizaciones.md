# Lección 10 — Ciclo comercial de cotizaciones

## Qué vas a conseguir

Convertirás una cotización estática en un pequeño flujo comercial explícito: `draft → sent → accepted|rejected`.

## Antes de empezar

Completa la [Lección 09](09-consultas-tipadas.md).

## El problema

Una cotización no sólo tiene importe. En trabajo real importa si todavía se está preparando, ya fue enviada o el cliente la aceptó/rechazó. Un `string` libre permite estados imposibles y transiciones ambiguas.

## Concepto

`QuoteStatus` representa el conjunto cerrado de estados conocidos. La tabla de transiciones expresa qué cambios son válidos. `parseQuoteStatus` protege datos externos y `changeQuoteStatus` conserva la regla pura.

## Demostración

[DEMO] Crea una cotización y comprueba que nace en `draft`. Intenta saltar directamente a `accepted`: debe fallar. Después envíala y acéptala.

```bash
npm test
```

## Código real

La API expone `PATCH /api/quotes/:id/status`. La ruta localiza la entidad, valida el estado solicitado y delega la transición al dominio. El estado terminal no puede reabrirse por accidente.

## Qué acaba de pasar

FreelanceDesk ya representa una decisión comercial y no sólo una estructura de datos.

## Errores comunes

- Permitir cualquier transición porque el tipo final es válido.
- Poner la matriz de transición únicamente en la UI.
- Hacer editable el `subtotal` en vez de recalcularlo desde conceptos.
- Tratar un estado desconocido como `draft` salvo que exista una política de compatibilidad explícita.

## Buenas prácticas

Mantén el estado cerca de la entidad, las transiciones en una función pura y la autorización/I-O fuera. Los estados terminales deben ser deliberados.

## Tu turno

Agrega la alternativa `sent → rejected` y una prueba que confirme que `accepted → rejected` sigue prohibido.

## Cómo comprobar

```bash
npm run verify
```

Crea una cotización por HTTP, envíala, acéptala y consulta `GET /api/quotes?status=accepted`.

## Solución enlazada

Usa el código canónico después de intentar el ejercicio.

## Reto adicional

¿Dónde pondrías una fecha de envío? Distingue dato de dominio, reloj externo y persistencia antes de escribir código.

## Resumen

Los unions discriminados por estado y las transiciones explícitas convierten reglas implícitas en contratos testeables.

## Siguiente paso

Continúa con la [Lección 11](11-mutaciones-durables-y-fallas-asincronas.md).

## Referencias

- [TypeScript — Union Types](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#union-types)
- [Node.js HTTP](https://nodejs.org/api/http.html)
