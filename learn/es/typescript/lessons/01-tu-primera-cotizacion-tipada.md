# Lección 01 — Tu primera cotización tipada

## Qué vas a conseguir

Ejecutarás TypeScript por primera vez y usarás tipos para calcular una cotización pequeña de FreelanceDesk sin esconder la lógica detrás de un framework.

## Antes de empezar

Instala Node.js 24 LTS y, desde `app/`, ejecuta `npm install`.

## El problema

Un freelancer recibe conceptos con cantidad y precio unitario. Una multiplicación equivocada o un valor con forma inesperada cambia el monto que presenta al cliente.

## Concepto

TypeScript comprueba tipos antes de ejecutar. `number`, `string`, arrays e interfaces permiten describir qué datos espera una operación. Esa comprobación no existe en el JavaScript final, pero reduce errores mientras escribes y modificas código.

## Demostración

[DEMO] Abre `src/domain/models.ts` y localiza `QuoteItem`. Observa que una línea tiene descripción, cantidad y precio. Después abre `src/domain/quotes.ts` y encuentra el cálculo del subtotal.

## Código real

`createQuote` recibe un `CreateQuoteInput`, valida conceptos y calcula `subtotal = suma(cantidad × precio unitario)`. La función no lee archivos, no toca el DOM y no conoce HTTP.

## Qué acaba de pasar

Usaste tipos para expresar el contrato de una regla real. El compilador puede detectar, por ejemplo, que intentas pasar texto donde el modelo exige un número.

## Errores comunes

- Creer que TypeScript convierte automáticamente texto de formularios en números.
- Usar `any` para silenciar el compilador.
- Mezclar cálculo con `console.log`, red o HTML.
- Pensar que una interface valida JSON en runtime.

## Buenas prácticas

Mantén reglas deterministas puras, usa nombres de dominio y deja I/O en los bordes. Prefiere tipos concretos antes que `any`.

## Tu turno

[PAUSA PARA EJERCICIO] Agrega una prueba que cotice tres unidades a 125.50 y compruebe el subtotal exacto. Luego intenta pasar `quantity: "3"` y observa el error del compilador antes de corregirlo.

## Cómo comprobar

```bash
npm run check
npm test
```

## Solución enlazada

Compara tu prueba con el estilo existente en `tests/domain.test.ts`; no necesitas copiar una solución separada todavía.

## Reto adicional

Explica por qué `number` no distingue pesos, dólares o porcentajes y qué riesgo tendría crear abstracciones monetarias demasiado pronto.

## Resumen

TypeScript agrega comprobación estática; las reglas puras permiten usar esa información sin acoplarla a I/O.

## Siguiente paso

Continúa con [Lección 02](02-modela-clientes-y-datos-de-negocio.md).

## Referencias

- [The Basics — TypeScript](https://www.typescriptlang.org/docs/handbook/2/basic-types.html)
- [Everyday Types — TypeScript](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html)
