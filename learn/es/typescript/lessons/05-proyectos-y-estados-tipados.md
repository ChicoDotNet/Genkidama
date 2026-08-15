# Lección 05 — Proyectos y estados que no se contradicen

## Qué vas a conseguir

Agregarás proyectos reales a FreelanceDesk y modelarás su ciclo de vida con un `union type` que impide estados arbitrarios dentro del código TypeScript.

## Antes de empezar

Completa la [Lección 04](04-de-tipos-a-app-full-stack-y-checkpoint.md).

## El problema

Clientes y cotizaciones no bastan para gestionar trabajo. Necesitamos saber qué proyecto está planeado, cuál está activo y cuál terminó. Una cadena libre como `status: string` permitiría `"terminadoo"`, `"pendiente?"` o cualquier valor accidental.

## Concepto

TypeScript puede representar un conjunto cerrado con:

```ts
type ProjectStatus = "planned" | "active" | "completed";
```

Eso documenta el contrato y mejora autocompletado. Pero recuerda: JSON externo no se vuelve seguro sólo porque nosotros anotemos un tipo. La validación runtime sigue siendo una frontera independiente.

## Demostración

[EN PANTALLA] Revisa `src/domain/models.ts` y `src/domain/projects.ts`. `createProject` normaliza nombre, exige cliente y siempre inicia en `planned`.

## Código real

```ts
const project = createProject("p1", { clientId: "c1", name: "Portal B2B" });
// project.status === "planned"
```

La función es pura: no conoce HTTP, archivos ni DOM.

## Qué acaba de pasar

Convertimos una palabra de negocio —estado— en un contrato verificable por compilador y pruebas.

## Errores comunes

- Usar `string` cuando el dominio tiene un conjunto cerrado.
- Permitir que quien crea el proyecto elija cualquier estado inicial.
- Meter llamadas a disco dentro de `createProject`.
- Confundir tipo estático con validación de JSON.

## Buenas prácticas

Modela primero reglas pequeñas y deterministas. Mantén identificadores como datos, no como dependencias entre objetos mutables.

## Tu turno

Agrega una prueba que demuestre que el nombre del proyecto se normaliza y que un nombre de un solo carácter se rechaza.

## Cómo comprobar

```bash
npm run verify
```

## Solución enlazada

Compara tu criterio con las pruebas existentes del dominio antes de avanzar; la solución del bloque llega en el Checkpoint 02.

## Reto adicional

¿Qué cambiaría si el producto necesitara un estado `cancelled`? Enumera qué transiciones permitirías y cuáles no.

## Resumen

Un `union type` expresa estados permitidos; la función de creación preserva el estado inicial y las reglas permanecen fuera de I/O.

## Siguiente paso

Continúa con [Lección 06 — Transiciones como regla de negocio](06-transiciones-de-proyecto-y-api.md).

## Referencias

- [TypeScript — Union Types](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#union-types)
- [TypeScript — Narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html)
