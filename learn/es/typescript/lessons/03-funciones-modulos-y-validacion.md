# Lección 03 — Funciones, módulos y validación

## Qué vas a conseguir

Separarás reglas en módulos pequeños, manejarás errores explícitos y comprenderás por qué los tipos no sustituyen validación runtime.

## Antes de empezar

Completa la [Lección 02](02-modela-clientes-y-datos-de-negocio.md).

## El problema

Los datos pueden llegar desde formularios o JSON. TypeScript no controla lo que envía otro proceso, un navegador modificado o un archivo externo.

## Concepto

El compilador sólo conoce el programa que analiza. En runtime, un `as CreateClientInput` no transforma ni valida datos. Por eso las fronteras reciben valores externos y ejecutan reglas antes de que el núcleo los trate como entidades válidas. Los módulos ESM mantienen dependencias explícitas y `import type` evita imports runtime innecesarios.

## Demostración

[DEMO] Cambia temporalmente un correo a `"ventas@"` en una prueba. Compila porque sigue siendo `string`, pero `createClient` lo rechaza en ejecución.

## Código real

`clients.ts` y `quotes.ts` contienen reglas puras. Los errores incluyen una causa accionable. `server/app.ts` convierte esos errores en respuestas HTTP sin meter HTTP dentro del dominio.

## Qué acaba de pasar

La arquitectura empieza a mostrar una frontera real: **entrada externa → validación/regla → entidad**.

## Errores comunes

- Usar `as` como mecanismo de validación.
- Capturar cualquier error y devolver éxito con datos incompletos.
- Exportar todo “por si acaso”.
- Crear helpers genéricos cuando una función de dominio clara basta.

## Buenas prácticas

Mantén exports explícitos, usa `unknown` en errores capturados, valida en la frontera adecuada y conserva mensajes útiles sin filtrar información sensible.

## Tu turno

Agrega una regresión para una cotización sin conceptos y exige un error útil. No cambies la prueba para aceptar cualquier excepción: verifica el contrato que te importa.

## Cómo comprobar

```bash
npm run verify
```

## Solución enlazada

Revisa las pruebas existentes sólo después de escribir la tuya.

## Reto adicional

Explica qué cambiaría si una API externa pudiera enviar `quantity: "2"`. ¿Dónde convertirías o rechazarías ese dato?

## Resumen

TypeScript protege durante desarrollo; la validación protege las fronteras durante ejecución.

## Siguiente paso

En [Lección 04](04-de-tipos-a-app-full-stack-y-checkpoint.md) conectarás dominio, HTTP y navegador.

## Referencias

- [Modules — TypeScript](https://www.typescriptlang.org/docs/handbook/modules/introduction.html)
- [Narrowing — TypeScript](https://www.typescriptlang.org/docs/handbook/2/narrowing.html)
