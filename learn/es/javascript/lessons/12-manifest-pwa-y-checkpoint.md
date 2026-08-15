# Lección 12 — Manifest, PWA y checkpoint 03

## Qué vas a conseguir
Completarás la primera PWA instalable y offline del curso, conectando persistencia local, app shell y metadata de aplicación. También aprenderás a distinguir lo que podemos verificar automáticamente de lo que necesita una prueba real en navegador.

## Antes de empezar
Ejecuta `npm run check`, `npm test` y `npm start`. Abre Application en las DevTools del navegador.

## El problema
Una app disponible offline no es automáticamente instalable. El navegador necesita metadata de aplicación, y una lista de cache aparentemente correcta puede quedar obsoleta cuando el grafo de módulos cambia.

## Concepto
`manifest.webmanifest` declara nombre, ruta inicial, alcance, modo `standalone`, colores e icono. `index.html` enlaza el manifest. El service worker mantiene la app shell. Son piezas relacionadas, pero no intercambiables.

La instalación y la experiencia offline dependen además del navegador y del contexto seguro. Por eso el curso conserva dos tipos de evidencia: CI para estructura reproducible y una auditoría manual para el comportamiento real de la PWA.

## Demostración
[DEMO] Revisa Manifest, Service Workers y Cache Storage en DevTools. Instala la aplicación si el navegador ofrece esa capacidad. Después pasa a offline y recarga.

[EJECUTAR] Corre `npm run check`: además de sintaxis, debe validar el manifest y comprobar que los módulos locales importados por `app.js` estén presentes en `APP_SHELL`.

## Código real
Kanban Local mantiene cuatro fronteras visibles:

- `board.js`: reglas puras del tablero;
- `repository.js` + persistencias: almacenamiento asíncrono y fallback;
- `app.js`: DOM y APIs de navegador;
- manifest + service worker: instalación y disponibilidad de la app shell.

El validador PWA no intenta fingir un navegador. Su responsabilidad es más pequeña: impedir que el shell se desincronice silenciosamente del código que la interfaz necesita para arrancar.

## Qué acaba de pasar
La aplicación ya puede conservar datos estructurados, volver a cargar su interfaz sin red después de una primera carga correcta y exponer metadata de instalación, todo usando APIs de plataforma y sin dependencia externa.

## Errores comunes
- asumir que el manifest por sí solo ofrece offline;
- declarar “Internet disponible” a partir de una señal que sólo describe estado de red del navegador;
- usar rutas que rompen al servir la carpeta desde otro origen/ruta;
- olvidar incluir un módulo nuevo en la app shell;
- considerar verde el CI como sustituto de una prueba offline real.

## Buenas prácticas
Mantén responsabilidades explícitas, rutas relativas coherentes, cache versionada y una separación honesta entre comprobaciones estáticas y comportamiento de navegador.

## Tu turno — Checkpoint 03
Resuelve [`../exercises/pwa-resilience-03.md`](../exercises/pwa-resilience-03.md) sin abrir la solución. Añadirás un diagnóstico de ejecución que distingue persistencia, preparación offline y estado de red reportado por el navegador; además harás evolucionar correctamente la app shell al introducir un módulo nuevo.

[PAUSA PARA EJERCICIO]

## Cómo comprobar
Ejecuta `npm run check`, `npm test` y `npm run smoke`. Después verifica manualmente primera carga online → service worker listo → modo offline → recarga.

## Solución
Compara con [`../solutions/pwa-resilience-03.md`](../solutions/pwa-resilience-03.md) cuando termines.

## Reto adicional
Diseña cómo informarías que existe una nueva versión de la app shell lista para activarse sin interrumpir el trabajo actual del usuario.

## Resumen
- manifest, Cache Storage e IndexedDB resuelven problemas distintos;
- una PWA necesita tanto contrato de plataforma como comportamiento observable;
- el app shell debe evolucionar junto con el grafo de módulos;
- automatización y prueba manual se complementan;
- el checkpoint integra asincronía, persistencia y offline sin introducir framework.

## Siguiente paso
Continúa con la [Lección 13 — Capas y comandos](13-capas-y-comandos.md).

## Referencias
- [Web app manifests — MDN](https://developer.mozilla.org/docs/Web/Progressive_web_apps/Manifest)
- [Progressive web apps — MDN](https://developer.mozilla.org/docs/Web/Progressive_web_apps)
