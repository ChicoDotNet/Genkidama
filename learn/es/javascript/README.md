# Curso de JavaScript desde cero — Construye un Kanban offline-first

Aprende JavaScript desde cero construyendo **Kanban Local**, una aplicación web que organiza tareas en columnas, conserva datos en el navegador y crecerá hasta funcionar como PWA offline-first.

## Qué es JavaScript y qué construirás

JavaScript es el lenguaje de programación nativo de la Web y también se usa en tooling y servicios mediante runtimes como Node.js. Aquí empiezas en el navegador para ver resultados desde la primera lección. Kanban Local permite crear tarjetas, moverlas entre `Por hacer`, `En curso` y `Terminado`, y conservar el tablero en `localStorage`.

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

Abre `http://127.0.0.1:4173`. No hay dependencias de runtime ni bundle en este primer bloque.

## Qué sabrás hacer al terminar

Leer y escribir JavaScript sencillo e idiomático; trabajar con objetos, arrays, funciones, módulos, DOM y eventos; manejar errores y persistencia; probar con `node:test`; comprender asincronía y APIs web; depurar y modificar una base existente; explicar decisiones en una entrevista junior.

## Ruta del curso

Estado actual: **4 de 17 lecciones implementadas**.

1. [Tu primer tablero en ejecución](lessons/01-tu-primer-tablero.md)
2. [Datos del tablero: objetos, arrays y render](lessons/02-datos-objetos-arrays-y-render.md)
3. [Funciones, módulos, eventos y persistencia](lessons/03-funciones-modulos-eventos-y-persistencia.md)
4. [Pruebas, validación y checkpoint 01](lessons/04-pruebas-validacion-y-checkpoint.md)
5. Edición y eliminación; 6. filtros y búsqueda; 7. accesibilidad/teclado; 8. JSON + checkpoint 02; 9. asincronía; 10. IndexedDB; 11. service worker; 12. manifest/PWA + checkpoint 03; 13. capas; 14. tooling; 15. debugging/rendimiento; 16. seguridad + checkpoint 04; 17. evaluación final.

## Checkpoint

Después de la lección 4: [`checkpoint-01`](exercises/checkpoint-01.md) y su [`solución de referencia`](solutions/checkpoint-01.md).

## Trabajo y alcance

Estas habilidades son base directa para frontend web y se transfieren a Node.js y frameworks como React, Vue o Angular. El curso entrega preparación práctica; no promete empleo ni sustituye experiencia real en equipos.

## FAQ

**¿Por qué no React desde el inicio?** Porque módulos, objetos, arrays, eventos, DOM y estado son fundamentos que un framework no reemplaza.

**¿Por qué Node.js si la app corre en navegador?** Para pruebas, chequeos y un servidor local reproducible.

**¿Por qué separar `localStorage`?** Para probar reglas sin navegador y facilitar la migración posterior a IndexedDB.

**¿Se enseña Git?** No; tendrá su propio curso.

## Glosario

- **DOM:** representación programable del documento.
- **Módulo ES:** archivo con imports/exports explícitos.
- **Evento:** señal como `click` o `submit`.
- **Estado:** datos actuales de la aplicación.
- **localStorage:** almacenamiento clave/valor del origen.
- **PWA:** aplicación web con capacidades instalables/offline cuando cumple requisitos de plataforma.

## Cómo hablar de este proyecto en una entrevista

Explica el problema antes del código: un tablero local sin backend. Luego describe la separación entre reglas (`board.js`), persistencia (`storage.js`) y DOM/eventos (`app.js`), y cómo `node:test` protege las reglas sin levantar un navegador completo.

## Referencias oficiales

- [JavaScript en MDN](https://developer.mozilla.org/docs/Web/JavaScript)
- [Módulos JavaScript](https://developer.mozilla.org/docs/Web/JavaScript/Guide/Modules)
- [`localStorage`](https://developer.mozilla.org/docs/Web/API/Window/localStorage)
- [`node:test`](https://nodejs.org/api/test.html)
- [Releases de Node.js](https://nodejs.org/en/about/previous-releases)

## Siguiente paso

Empieza con la [Lección 1](lessons/01-tu-primer-tablero.md).
