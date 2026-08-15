# Curso de JavaScript desde cero — Construye un Kanban offline-first

Aprende JavaScript desde cero construyendo **Kanban Local**, una aplicación web local que organiza tareas, conserva datos en el navegador y añade capacidades progresivas de uso sin conexión.

## Tooling verificado
- Node.js **24.18.1 LTS** para CI, pruebas y servidor local.
- npm incluido con Node.js; sin dependencias de terceros en este curso.
- Navegador moderno con APIs web actuales.

## Instalar, Build, Test y Run
Desde `app/` ejecuta `npm run verify` y después `npm start`. Abre `http://127.0.0.1:4173`.

`npm run verify` compone sintaxis/integridad PWA, toda la suite `node:test` y el smoke del dominio. El workflow path-scoped reutiliza ese mismo contrato y agrega comprobación HTTP de manifest, service worker, MIME types y headers defensivos. La prueba real online/offline sigue siendo una comprobación de navegador, no una afirmación fabricada por CI.

## Ruta del curso
Estado: **17 de 17 lecciones implementadas**.

1. [Tu primer tablero](lessons/01-tu-primer-tablero.md)
2. [Objetos, arrays y render](lessons/02-datos-objetos-arrays-y-render.md)
3. [Funciones, módulos, eventos y persistencia](lessons/03-funciones-modulos-eventos-y-persistencia.md)
4. [Pruebas y checkpoint 01](lessons/04-pruebas-validacion-y-checkpoint.md)
5. [Editar y eliminar](lessons/05-editar-y-eliminar.md)
6. [Filtros y búsqueda](lessons/06-filtros-y-busqueda.md)
7. [Accesibilidad y teclado](lessons/07-accesibilidad-y-teclado.md)
8. [JSON y checkpoint 02](lessons/08-importar-exportar-json-y-checkpoint.md)
9. [Asincronía real](lessons/09-asincronia-real.md)
10. [Persistencia estructurada](lessons/10-persistencia-estructurada.md)
11. [App shell offline](lessons/11-app-shell-offline.md)
12. [Manifest/PWA y checkpoint 03](lessons/12-manifest-pwa-y-checkpoint.md)
13. [Capas y comandos](lessons/13-capas-y-comandos.md)
14. [Tooling y gate profesional](lessons/14-tooling-y-gate-profesional.md)
15. [Diagnóstico y rendimiento](lessons/15-diagnostico-y-rendimiento.md)
16. [Seguridad, hardening y checkpoint 04](lessons/16-seguridad-hardening-y-checkpoint.md)
17. [Evaluación final sin receta](lessons/17-evaluacion-final.md)

## Checkpoints
- [Checkpoint 01](exercises/checkpoint-01.md) · [solución](solutions/checkpoint-01.md)
- [Checkpoint 02](exercises/checkpoint-02.md) · [solución](solutions/checkpoint-02.md)
- [Checkpoint 03](exercises/pwa-resilience-03.md) · [solución](solutions/pwa-resilience-03.md)
- [Checkpoint 04](exercises/importacion-segura-04.md) · [solución](solutions/importacion-segura-04.md)

## Evaluación final
La evaluación final no prescribe archivos ni pasos de implementación. Exige leer una base existente, agregar prioridad compatible con datos anteriores, corregir una importación ambigua, escribir regresiones, conservar persistencia/PWA/hardening, consultar documentación oficial y defender decisiones de diseño.

- [Evaluación final](exercises/evaluacion-final.md)
- [Rúbrica de 100 puntos](exercises/rubrica-final.md)
- [Solución de referencia](solutions/evaluacion-final.md) — sólo después de un intento propio.

## Qué sabrás hacer al terminar
Leer y escribir JavaScript sencillo e idiomático; trabajar con estado, DOM/eventos, errores, persistencia, asincronía y APIs web; separar dominio/aplicación/I/O; instrumentar problemas antes de optimizar; tratar entradas como fronteras; escribir pruebas con `node:test`; modificar una base existente y explicar arquitectura en una entrevista junior.

## Cómo hablar de este proyecto en una entrevista
Presenta primero el problema: un Kanban local que debe seguir funcionando sin backend y tolerar evolución de datos. Después explica decisiones concretas: JavaScript nativo para dominar la plataforma, dominio separado del DOM, comandos testeables, IndexedDB con fallback/compatibilidad, service worker + app shell, importación como frontera no confiable, diagnóstico opt-in y un gate único `npm run verify` compartido con CI.

También menciona límites reales: una PWA local no resuelve sincronización multiusuario; `navigator.onLine` no prueba alcance de un servidor; CSP no sustituye validación; y una medición local no demuestra rendimiento en todos los dispositivos.

## Trabajo y alcance
Estas habilidades son base directa de frontend web y se transfieren a Node.js y frameworks. El curso prepara práctica demostrable; no promete empleo ni sustituye experiencia de equipo.

## FAQ
**¿Por qué no React desde el inicio?** Porque los fundamentos de JavaScript y plataforma web son transferibles.

**¿Por qué no agregar ESLint/Prettier/Playwright sólo para enseñar tooling?** Porque una dependencia debe resolver un problema concreto. El curso primero enseña a construir un gate reproducible con las herramientas ya justificadas; dependencias adicionales pueden incorporarse cuando compren una capacidad real.

**¿`npm run verify` demuestra que la PWA funciona offline?** No. Detecta incoherencias estructurales y regresiones ejecutables. El service worker y la recarga offline se verifican además en un navegador.

**¿Dónde aprendo Git?** En el [curso transversal de Git](../git/). JavaScript no duplica aquí ramas, merges, recuperación o colaboración; esa ruta compartida enseña Git sobre un laboratorio independiente.

## Referencias oficiales
- [JavaScript — MDN](https://developer.mozilla.org/docs/Web/JavaScript)
- [IndexedDB — MDN](https://developer.mozilla.org/docs/Web/API/IndexedDB_API)
- [Progressive web apps — MDN](https://developer.mozilla.org/docs/Web/Progressive_web_apps)
- [Performance APIs — MDN](https://developer.mozilla.org/docs/Web/API/Performance)
- [Content Security Policy — MDN](https://developer.mozilla.org/docs/Web/HTTP/CSP)
- [`node:test`](https://nodejs.org/api/test.html)

## Siguiente paso
Completa la [evaluación final](exercises/evaluacion-final.md), usa la [rúbrica](exercises/rubrica-final.md) para detectar huecos y después construye una variante propia sin copiar la solución de referencia.
