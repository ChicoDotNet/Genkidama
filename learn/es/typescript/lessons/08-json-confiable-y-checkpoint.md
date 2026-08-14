# Lección 08 — JSON confiable y Checkpoint 02

## Qué vas a conseguir

Persistirás FreelanceDesk en JSON sin confiar ciegamente en `JSON.parse`, y cerrarás el bloque con una extensión que protege el ciclo de vida de proyectos.

## Antes de empezar

Completa la [Lección 07](07-frontera-de-persistencia.md).

## El problema

`JSON.parse` devuelve datos de runtime. Una anotación TypeScript no puede garantizar que un archivo antiguo, editado a mano o corrupto tenga `clients`, `quotes`, `projects` ni estados permitidos.

## Concepto

La persistencia tiene dos responsabilidades distintas:

1. **serializar** un snapshot conocido;
2. **validar** datos desconocidos al cargarlos.

`parseSnapshot` comienza desde `unknown` y comprueba forma, tipos primitivos y estados. Si el archivo no existe, el estado vacío es una situación normal. Si el archivo existe pero su contenido viola el contrato, se falla explícitamente.

## Demostración

[EJECUTAR]

```bash
npm test
```

La suite crea directorios temporales, guarda un snapshot, lo recarga y también verifica que un proyecto con estado `paused` sea rechazado.

## Código real

`JsonFileStateStore.save` escribe primero un archivo temporal y después lo renombra. Así evitamos reemplazar el archivo final con contenido a medio escribir si una escritura falla antes de terminar.

El servidor real carga `data/freelance-desk.json` al arrancar. Puedes cambiar la ruta con `FREELANCEDESK_DATA_FILE`.

## Qué acaba de pasar

FreelanceDesk ya conserva datos entre reinicios y puede distinguir “primera ejecución” de “persistencia corrupta”.

## Errores comunes

- Escribir `JSON.parse(text) as AppSnapshot` y asumir que eso valida.
- Convertir cualquier error de lectura en estado vacío, escondiendo corrupción o permisos.
- Sobrescribir directamente el archivo final sin estrategia de reemplazo.
- Hacer que las funciones de dominio conozcan rutas del sistema operativo.

## Buenas prácticas

Trata archivos como entrada no confiable. Conserva errores útiles, pruebas con directorios temporales y una frontera que mañana pueda cambiar a SQLite sin reescribir reglas de negocio.

## Tu turno — Checkpoint 02

Resuelve [`../exercises/checkpoint-02.md`](../exercises/checkpoint-02.md) sin abrir la solución. Debes agregar una transición explícita `active → planned` para devolver un proyecto a planificación, sin permitir reabrir un proyecto `completed`, y proteger la nueva regla con pruebas de dominio y HTTP.

## Cómo comprobar

```bash
npm run verify
npm start
```

Crea cliente y proyecto, actívalo, reinicia el servidor y confirma que el proyecto continúa disponible en `GET /api/projects`.

## Solución enlazada

Después de tu intento consulta [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md).

## Reto adicional

¿Qué necesitarías para hacer migraciones de esquema cuando un snapshot futuro agregue campos obligatorios? No lo implementes todavía; define versión y política de compatibilidad.

## Resumen

Persistir no es sólo escribir bytes: es definir un contrato de datos, validar al leer y fallar de forma controlada cuando la realidad no coincide con los tipos.

## Siguiente paso

Continúa con la [Lección 09](09-consultas-tipadas.md) para convertir el estado persistido en consultas útiles y validadas.

## Referencias

- [Node.js File system promises](https://nodejs.org/api/fs.html#promises-api)
- [TypeScript — Narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html)
- [JSON.parse — MDN](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse)
