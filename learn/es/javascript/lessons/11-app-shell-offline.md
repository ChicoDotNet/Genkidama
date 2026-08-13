# Lección 11 — App shell disponible offline

## Qué vas a conseguir
Harás que la interfaz de Kanban Local pueda volver a abrir sin conexión después de una primera carga correcta y entenderás qué está protegiendo el service worker.

## Antes de empezar
Sirve la app con `npm start`, abre las herramientas del navegador y ejecuta `npm run check` una vez.

## El problema
Persistir datos no basta si el navegador no puede volver a cargar HTML, CSS y módulos JavaScript. Una aplicación puede conservar información perfectamente y aun así no arrancar sin red.

## Concepto
Un service worker vive fuera del documento y puede interceptar solicitudes del mismo origen. Durante `install`, Kanban Local precachea una **app shell**: los recursos mínimos necesarios para iniciar la interfaz. Durante `activate`, elimina caches de versiones anteriores. En `fetch`, usa primero la copia cacheada y conserva respuestas GET válidas del mismo origen.

Cache Storage no sustituye IndexedDB: una conserva archivos/respuestas de la aplicación; la otra conserva datos estructurados del usuario.

## Demostración
[DEMO] Abre la app una vez, revisa Application → Cache Storage, activa modo offline en DevTools y recarga. La interfaz debe volver a aparecer y el tablero debe conservarse.

## Código real
`service-worker.js` declara `CACHE_NAME` y `APP_SHELL`. Ahí aparecen `index.html`, estilos, manifest, icono y los módulos importados por la aplicación.

`npm run check` ejecuta además un validador estático que recorre los imports locales desde `src/app.js` y verifica que cada módulo necesario esté dentro de `APP_SHELL`. No reemplaza la prueba manual del navegador, pero evita una clase frecuente de regresión: agregar un módulo y olvidar precachearlo.

## Qué acaba de pasar
Offline separa al menos dos problemas: **disponibilidad del código** y **persistencia de los datos**. Service worker + Cache Storage ayudan con el primero; IndexedDB/localStorage con el segundo.

## Errores comunes
- cachear recursos sin criterio;
- olvidar versionar la cache cuando cambia la app shell;
- agregar un módulo importado y no incorporarlo al precache;
- creer que Cache Storage es una base de datos de dominio;
- probar sólo con la red disponible.

## Buenas prácticas
Mantén la app shell pequeña, usa rutas coherentes con el origen, elimina caches antiguas durante activación y conserva una comprobación automatizada de integridad además de la prueba offline real.

## Tu turno
Agrega temporalmente un módulo local importado desde `app.js` sin incorporarlo a `APP_SHELL` y ejecuta `npm run check`. Observa el fallo. Después corrige el shell y vuelve a verde.

## Cómo comprobar
Ejecuta `npm run check`, luego prueba online → modo offline → recarga en un navegador. Ambas comprobaciones responden preguntas distintas.

## Solución
El mensaje del validador te indica qué asset importado falta en `APP_SHELL`. No desactives el gate: corrige la lista.

## Reto adicional
Compara cache-first y network-first y explica qué estrategia elegirías para una API remota que cambie con frecuencia.

## Resumen
- el service worker mantiene disponible la app shell;
- Cache Storage y la base local cumplen responsabilidades distintas;
- el grafo de módulos y el precache deben evolucionar juntos;
- CI puede detectar incoherencia estructural, pero la experiencia offline requiere prueba real del navegador.

## Siguiente paso
Continúa con [Lección 12 — Manifest, PWA y checkpoint 03](12-manifest-pwa-y-checkpoint.md).

## Referencias
- [Service Worker API — MDN](https://developer.mozilla.org/docs/Web/API/Service_Worker_API)
- [Cache — MDN](https://developer.mozilla.org/docs/Web/API/Cache)
