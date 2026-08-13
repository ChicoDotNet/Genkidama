# Curso de JavaScript desde cero — Construye un Kanban offline-first

Aprende JavaScript desde cero construyendo **Kanban Local**, una aplicación web que organiza tareas en columnas, conserva datos en el navegador y crecerá hasta funcionar como PWA offline-first.

## Qué es JavaScript y qué construirás

JavaScript es el lenguaje de programación nativo de la Web y también se usa en tooling y servicios mediante runtimes como Node.js. Aquí empiezas en el navegador para ver resultados desde la primera lección. Kanban Local permite crear, editar, buscar y mover tarjetas entre `Por hacer`, `En curso` y `Terminado`, conservar el tablero en `localStorage` y respaldarlo/restaurarlo mediante JSON.

## Tooling verificado

- Node.js **24.18.1 LTS** para CI, pruebas y servidor local.
- npm incluido con Node.js.
- Navegador moderno con módulos ES y `localStorage`.
- Windows 11 + PowerShell + VS Code; Linux + bash + VS Code como alternativa.

Node.js 26 sigue siendo Current en esta verificación; el curso usa la línea 24 LTS para continuidad educativa y productiva.

## Instalar, Build, Test y Run

Desde `app/`:

```bash
node --version
npm run check
npm test
npm start
```

Abre `http://127.0.0.1:4173`. El curso sigue sin dependencias de runtime ni de desarrollo de terceros: en este bloque usamos JavaScript, Web Platform APIs y el runner nativo de Node.js.

## Qué sabrás hacer al terminar

Leer y escribir JavaScript sencillo e idiomático; trabajar con objetos, arrays, funciones, módulos, DOM y eventos; manejar errores y persistencia; probar con `node:test`; comprender asincronía y APIs web; depurar y modificar una base existente; explicar decisiones en una entrevista junior.

## Ruta del curso

Estado actual: **8 de 17 lecciones implementadas**.

1. [Tu primer tablero en ejecución](lessons/01-tu-primer-tablero.md)
2. [Datos del tablero: objetos, arrays y render](lessons/02-datos-objetos-arrays-y-render.md)
3. [Funciones, módulos, eventos y persistencia](lessons/03-funciones-modulos-eventos-y-persistencia.md)
4. [Pruebas, validación y checkpoint 01](lessons/04-pruebas-validacion-y-checkpoint.md)
5. [Editar y eliminar sin convertir el DOM en tu base de datos](lessons/05-editar-y-eliminar.md)
6. [Filtros y búsqueda: derivar una vista sin destruir datos](lessons/06-filtros-y-busqueda.md)
7. [Accesibilidad y teclado: una UI que no depende de arrastrar](lessons/07-accesibilidad-y-teclado.md)
8. [Importar/exportar JSON y checkpoint 02](lessons/08-importar-exportar-json-y-checkpoint.md)
9. Asincronía; 10. IndexedDB; 11. service worker; 12. manifest/PWA + checkpoint 03; 13. capas; 14. tooling; 15. debugging/rendimiento; 16. seguridad + checkpoint 04; 17. evaluación final.

## Checkpoints

- Después de la lección 4: [`checkpoint-01`](exercises/checkpoint-01.md) y su [`solución de referencia`](solutions/checkpoint-01.md).
- Después de la lección 8: [`checkpoint-02`](exercises/checkpoint-02.md) y su [`solución de referencia`](solutions/checkpoint-02.md).

## Trabajo y alcance

Estas habilidades son base directa para frontend web y se transfieren a Node.js y frameworks como React, Vue o Angular. El curso entrega preparación práctica; no promete empleo ni sustituye experiencia real en equipos.

## FAQ

**¿Por qué no React desde el inicio?** Porque módulos, objetos, arrays, eventos, DOM y estado son fundamentos que un framework no reemplaza.

**¿Por qué Node.js si la app corre en navegador?** Para pruebas, chequeos y un servidor local reproducible.

**¿Por qué separar `localStorage`?** Para probar reglas sin navegador y facilitar la migración posterior a IndexedDB.

**¿Por qué exportar JSON si ya existe `localStorage`?** Porque almacenamiento local y formato portable resuelven problemas distintos. El archivo permite respaldo/traslado y obliga a practicar validación de una frontera externa.

**¿Por qué no drag-and-drop todavía?** Porque mover tarjetas debe ser operable con teclado desde el principio. Si añadimos drag-and-drop después, será una mejora y no la única ruta.

**¿Se enseña Git?** No; tendrá su propio curso.

## Glosario

- **DOM:** representación programable del documento.
- **Módulo ES:** archivo con imports/exports explícitos.
- **Evento:** señal como `click` o `submit`.
- **Estado:** datos actuales de la aplicación.
- **Vista derivada:** datos calculados para presentar sin modificar la fuente de verdad.
- **localStorage:** almacenamiento clave/valor del origen.
- **JSON:** formato de texto para representar datos estructurados; no implica por sí mismo que los datos sean válidos.
- **Región viva:** zona que puede anunciar cambios a tecnología asistiva.
- **PWA:** aplicación web con capacidades instalables/offline cuando cumple requisitos de plataforma.

## Cómo hablar de este proyecto en una entrevista

Explica el problema antes del código: un tablero local sin backend. Describe la separación entre reglas (`board.js`), persistencia/serialización (`storage.js`) y DOM/eventos (`app.js`). Puedes explicar por qué editar/eliminar se implementan como transformaciones del estado, por qué búsqueda es una vista derivada, por qué existe una alternativa de teclado a cualquier interacción visual y por qué importar JSON exige validación adicional a `JSON.parse`. `node:test` protege esas reglas sin levantar un navegador completo.

## Referencias oficiales

- [JavaScript en MDN](https://developer.mozilla.org/docs/Web/JavaScript)
- [Módulos JavaScript](https://developer.mozilla.org/docs/Web/JavaScript/Guide/Modules)
- [`localStorage`](https://developer.mozilla.org/docs/Web/API/Window/localStorage)
- [`JSON.parse`](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse)
- [`Blob`](https://developer.mozilla.org/docs/Web/API/Blob)
- [WAI — Introduction to Web Accessibility](https://www.w3.org/WAI/fundamentals/accessibility-intro/)
- [`node:test`](https://nodejs.org/api/test.html)
- [Releases de Node.js](https://nodejs.org/en/about/previous-releases)

## Siguiente paso

Empieza con la [Lección 1](lessons/01-tu-primer-tablero.md).
