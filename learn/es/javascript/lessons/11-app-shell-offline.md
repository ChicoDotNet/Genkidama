# Lección 11 — App shell disponible offline

## Qué vas a conseguir
Harás que la interfaz de Kanban Local pueda volver a abrir sin conexión después de una primera carga correcta.

## Antes de empezar
Sirve la app con `npm start` y abre las herramientas del navegador.

## El problema
Persistir datos no basta si el navegador no puede volver a cargar los archivos de la aplicación.

## Concepto
Un service worker puede usar Cache Storage para conservar los recursos esenciales de la app. Durante instalación se guarda una app shell y durante activación se limpian versiones antiguas.

## Demostración
[DEMO] Abre la app una vez, activa el modo offline de DevTools y recarga.

## Código real
`service-worker.js` usa recursos GET del mismo origen y un nombre de cache versionado.

## Qué acaba de pasar
La experiencia offline separa disponibilidad del código y persistencia de datos. Cache Storage ayuda con lo primero; IndexedDB y localStorage con lo segundo.

## Errores comunes
Guardar recursos sin criterio; olvidar versionar la cache; confundir cache con base de datos.

## Buenas prácticas
Mantén la app shell pequeña y elimina versiones antiguas durante activación.

## Tu turno
Enumera qué recursos forman la app shell actual y por qué cada uno es necesario.

## Cómo comprobar
`npm run check`; después prueba online, offline y recarga.

## Solución
Revisa `APP_SHELL` y los eventos de `service-worker.js`.

## Reto adicional
Compara cache-first y network-first.

## Resumen
El service worker mantiene disponible la app shell sin conexión.

## Siguiente paso
Continúa con [Lección 12 — Manifest, PWA y checkpoint 03](12-manifest-pwa-y-checkpoint.md).

## Referencias
- [Service Worker API — MDN](https://developer.mozilla.org/docs/Web/API/Service_Worker_API)
- [Cache — MDN](https://developer.mozilla.org/docs/Web/API/Cache)
