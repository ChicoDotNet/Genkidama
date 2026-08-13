# Lección 13 — Capas y comandos sin sobrearquitectura

## Qué vas a conseguir
Separarás la intención de la interfaz de las reglas del tablero mediante una pequeña capa de comandos. El objetivo no es aprender nombres sofisticados, sino poder probar una modificación del sistema sin montar el DOM ni tocar almacenamiento.

## Antes de empezar
Desde `app/` ejecuta `npm run verify`. Revisa `src/app.js`, `src/board.js`, `src/repository.js` y el nuevo `src/commands.js`.

## El problema
Hasta ahora `app.js` sabía demasiado sobre cómo ejecutar cada operación de negocio. Un click de interfaz terminaba llamando directamente `addCard`, `editCard`, `moveCard` o `deleteCard`. Eso funciona en una app pequeña, pero mezcla dos preguntas distintas:

1. ¿Qué quiso hacer el usuario?
2. ¿Cómo cambia el estado del tablero cuando ocurre esa intención?

Cuando ambas respuestas viven en el mismo archivo, probar reglas desde otro canal —por ejemplo una futura CLI, sincronización o automatización— obliga a repetir lógica de coordinación.

## Concepto
Una **capa de aplicación** puede ser muy pequeña. En Kanban Local es una función: `applyBoardCommand(board, command)`.

El comando representa intención con datos simples:

```js
{ type: "move", cardId: "abc", targetColumn: "done" }
```

`commands.js` decide qué operación de dominio corresponde. `board.js` sigue validando y transformando el estado. `app.js` se queda con DOM, prompts, eventos y mensajes al usuario. La persistencia continúa detrás de `repository.js`.

No hemos creado clases, contenedores de inyección ni un bus de mensajes. La separación existe porque resuelve un problema observable, no porque un diagrama diga que toda app necesita cinco capas.

## Demostración
[DEMO] Abre `tests/commands.test.js`. Observa que podemos ejecutar una secuencia completa de agregar, mover, editar y eliminar sin `document`, `window`, IndexedDB ni servidor.

[EJECUTAR]

```powershell
npm test
```

Busca las pruebas del módulo de comandos.

## Código real
La frontera central tiene esta forma:

```js
export function applyBoardCommand(board, command) {
  switch (command.type) {
    case "add":
      return addCard(board, command.title, command.id);
    case "edit":
      return editCard(board, command.cardId, command.title);
    case "move":
      return moveCard(board, command.cardId, command.targetColumn);
    case "delete":
      return deleteCard(board, command.cardId);
    default:
      throw new RangeError(`Comando desconocido: ${command.type}.`);
  }
}
```

La función no conoce botones ni almacenamiento. Tampoco duplica reglas: delega al dominio existente.

## Qué acaba de pasar
Ahora podemos describir la arquitectura con responsabilidades concretas:

- **dominio (`board.js`)**: invariantes y transformaciones puras;
- **aplicación (`commands.js`)**: traduce intención a una operación de dominio;
- **infraestructura (`repository.js`, storage e IndexedDB)**: conserva estado;
- **presentación (`app.js`)**: DOM y APIs del navegador;
- **plataforma**: manifest, service worker y servidor local.

La prueba profesional no es que el proyecto tenga carpetas llamadas `domain` o `application`; es que una responsabilidad pueda cambiar sin obligar a reescribir las otras.

## Errores comunes
- mover reglas de negocio a `commands.js` y duplicarlas;
- crear una clase por cada acción sólo para “tener arquitectura”;
- permitir que `board.js` acceda al DOM o a `localStorage`;
- esconder errores con `try/catch` en todas las capas;
- cambiar varias fronteras a la vez sin pruebas que expliquen por qué.

## Buenas prácticas
Mantén comandos como datos pequeños, errores explícitos e invariantes en el dominio. Prefiere una frontera mínima que puedas explicar a una abstracción que sólo añade ceremonias.

## Tu turno
Agrega una prueba en `commands.test.js` que demuestre que un comando desconocido falla de forma controlada. Después escribe, sin código, cómo añadirías una segunda interfaz —por ejemplo una CLI— reutilizando `commands.js` sin importar `app.js`.

[PAUSA PARA EJERCICIO]

## Cómo comprobar
Ejecuta `npm run verify`. Ninguna prueba debería requerir un navegador para validar los comandos.

## Solución
Una respuesta razonable conserva `board.js` como fuente de reglas y hace que la nueva interfaz construya comandos. No necesitas crear una arquitectura nueva.

## Reto adicional
¿Qué ganamos y qué perdemos si cada comando se convierte en una función independiente en lugar de usar un `switch` central? Argumenta desde este proyecto, no desde una regla universal.

## Resumen
- una capa es una frontera de responsabilidad, no una carpeta decorativa;
- los comandos expresan intención sin depender del canal que la originó;
- el dominio sigue siendo dueño de sus invariantes;
- la separación correcta aumenta testabilidad sin inflar el diseño;
- arquitectura junior saludable significa poder explicar por qué existe cada frontera.

## Siguiente paso
Continúa con la [Lección 14 — Un gate profesional y reproducible](14-tooling-y-gate-profesional.md).

## Referencias
- [JavaScript modules — MDN](https://developer.mozilla.org/docs/Web/JavaScript/Guide/Modules)
- [Test runner — Node.js](https://nodejs.org/api/test.html)
