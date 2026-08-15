# Lección 06 — Transiciones de proyecto y API

## Qué vas a conseguir

Harás explícita la regla `planned → active → completed` y la expondrás mediante HTTP sin permitir saltos ni valores inventados por JSON externo.

## Antes de empezar

Completa la [Lección 05](05-proyectos-y-estados-tipados.md).

## El problema

Un proyecto no debería pasar de planeado a terminado sin haber estado activo. Tampoco debería reabrirse silenciosamente después de `completed`. Si cada controlador decide por su cuenta, la regla se duplica y termina divergiendo.

## Concepto

La transición pertenece al dominio. `changeProjectStatus` conoce el grafo permitido; HTTP sólo localiza el proyecto, valida el valor externo y delega la decisión.

El compilador protege llamadas internas, pero el cuerpo HTTP llega como datos desconocidos. Por eso `parseProjectStatus` recibe `unknown` y devuelve `ProjectStatus` sólo después de comprobarlo.

## Demostración

[EJECUTAR]

```bash
npm test
```

Observa dos niveles de prueba: el dominio impide saltos y la API devuelve `400` tanto para una transición inválida como para `"paused"`.

## Código real

La ruta es deliberadamente pequeña:

```text
PATCH /api/projects/:id/status
{ "status": "active" }
```

No contiene la tabla de transiciones. Esa regla permanece en `src/domain/projects.ts`.

## Qué acaba de pasar

La API puede evolucionar sin convertirse en dueña de la semántica del negocio. TypeScript ayuda dentro del programa y la validación runtime protege la frontera.

## Errores comunes

- Usar `as ProjectStatus` para silenciar datos externos.
- Duplicar el `if` de transiciones en UI, API y persistencia.
- Mutar el objeto original y romper pruebas que conservan referencias.
- Devolver `500` para un error de entrada conocido.

## Buenas prácticas

Convierte entradas externas a tipos confiables en un punto claro. Haz que las funciones de dominio devuelvan nuevos valores y deja I/O en adaptadores.

## Tu turno

Agrega una prueba para intentar pasar de `completed` a `active`. Debe fallar con un mensaje útil.

## Cómo comprobar

```bash
npm run verify
```

Después crea un proyecto por API, actívalo y complétalo con dos `PATCH` consecutivos.

## Solución enlazada

La solución de referencia del bloque se encuentra en [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md) después de completar el checkpoint.

## Reto adicional

Diseña cómo responderías con códigos HTTP diferentes para “proyecto inexistente” y “transición inválida” sin mover reglas al transporte.

## Resumen

Los estados tipados no bastan: una transición es una regla de negocio, y el JSON necesita validación runtime antes de entrar al dominio.

## Siguiente paso

Continúa con [Lección 07 — Una frontera de persistencia](07-frontera-de-persistencia.md).

## Referencias

- [TypeScript — `unknown`](https://www.typescriptlang.org/docs/handbook/2/functions.html#unknown)
- [Node.js HTTP](https://nodejs.org/api/http.html)
