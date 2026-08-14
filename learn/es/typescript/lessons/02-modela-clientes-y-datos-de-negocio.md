# Lección 02 — Modela clientes y datos de negocio

## Qué vas a conseguir

Modelarás clientes, cotizaciones e identificadores con aliases e interfaces, distinguiendo datos internos de entradas externas.

## Antes de empezar

Completa la [Lección 01](01-tu-primera-cotizacion-tipada.md) y conserva `npm run check` verde.

## El problema

Un objeto `{ name, email }` no significa lo mismo antes y después de validarlo. Además, una cotización debe referirse a un cliente estable sin copiar todo el objeto por todas partes.

## Concepto

Un `type` puede nombrar una unión, alias o forma; una `interface` describe con claridad contratos de objeto ampliables. No existe una regla de “interface siempre” o “type siempre”: elegimos la forma que expresa mejor el contrato. `readonly` comunica que una entidad creada no debería mutarse accidentalmente desde otro módulo.

## Demostración

[EN PANTALLA] Recorre `models.ts`: `EntityId`, `Client`, `QuoteItem`, `Quote`, `CreateClientInput` y `CreateQuoteInput`. Identifica qué tipos representan entidades ya aceptadas y cuáles representan datos que todavía deben pasar una frontera.

## Código real

`createClient` toma un `CreateClientInput`, normaliza nombre/correo y devuelve un `Client`. La función nunca confía en que un objeto tenga semántica correcta sólo porque compiló.

## Qué acaba de pasar

El modelo separa **forma estática** de **validez de negocio**. TypeScript documenta relaciones; las funciones de dominio establecen reglas.

## Errores comunes

- Reutilizar el mismo tipo para entrada cruda y entidad validada cuando tienen garantías distintas.
- Usar IDs numéricos incrementales sólo porque son fáciles en memoria.
- Mutar arrays recibidos por una función.
- Convertir cada string en un tipo nominal antes de que exista riesgo real.

## Buenas prácticas

Usa `readonly` para contratos que no deben mutarse; modela sólo distinciones que tengan significado; documenta exports públicos con TSDoc/JSDoc.

## Tu turno

Agrega un caso que demuestre que el correo se normaliza a minúsculas y espacios exteriores se eliminan. Después intenta modificar `client.name` desde una prueba y observa la protección estática.

## Cómo comprobar

```bash
npm run check
npm test
```

## Solución enlazada

El comportamiento esperado está protegido por `tests/domain.test.ts`.

## Reto adicional

Propón un tipo para `ProjectStatus` con tres estados posibles sin crear todavía la funcionalidad de proyectos.

## Resumen

Los tipos expresan relaciones; la validación crea garantías que el compilador por sí solo no puede inferir de datos externos.

## Siguiente paso

En [Lección 03](03-funciones-modulos-y-validacion.md) convertirás esas garantías en fronteras reutilizables.

## Referencias

- [Object Types — TypeScript](https://www.typescriptlang.org/docs/handbook/2/objects.html)
- [Creating Types from Types](https://www.typescriptlang.org/docs/handbook/2/types-from-types.html)
