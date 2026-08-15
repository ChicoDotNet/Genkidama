# Checkpoint 03 — Diagnóstico honesto del modo offline

## Objetivo

Añade un diagnóstico de ejecución que distinga tres hechos que no son equivalentes:

1. dónde quedó persistido el tablero (`indexeddb` o `localstorage`);
2. si el service worker ya está listo para servir la app shell;
3. si el navegador reporta estado de red en línea o sin conexión.

La UI debe informar esos hechos sin convertirlos en reglas del dominio ni afirmar que existe Internet sólo porque `navigator.onLine` sea `true`.

## Requisitos

1. Crea `src/runtime-status.js` con una función pública pura `formatRuntimeStatus({ persistenceMode, offlineReady, online })`.
2. La función debe:
   - aceptar únicamente `indexeddb` o `localstorage` como modo de persistencia;
   - diferenciar app shell confirmada (`offlineReady: true`) de todavía no confirmada;
   - describir `online` como el **estado que reporta el navegador**, no como prueba de que un servidor sea alcanzable;
   - no acceder a `window`, `navigator`, `document`, IndexedDB ni Cache Storage.
3. En `app.js`:
   - usa la función para el mensaje operativo;
   - registra el service worker como hasta ahora;
   - considera `offlineReady = true` sólo después de que `navigator.serviceWorker.ready` se resuelva;
   - escucha los eventos `online` y `offline` para refrescar el diagnóstico sin modificar el tablero.
4. Añade pruebas con `node:test` para al menos:
   - IndexedDB + app shell lista + navegador en línea;
   - localStorage + app shell aún no confirmada + navegador sin conexión;
   - modo de persistencia desconocido rechazado explícitamente.
5. Como `app.js` importará un módulo nuevo:
   - agrega `./src/runtime-status.js` a `APP_SHELL`;
   - cambia el nombre de cache a una nueva versión para que el service worker no confunda shells distintos.
6. Mantén verdes `npm run check`, `npm test` y `npm run smoke`.
7. Verifica manualmente: primera carga online → service worker listo → modo offline en DevTools → recarga.

## Restricciones

- No hagas `fetch` a un servidor sólo para decidir si hay Internet.
- No muevas esta lógica a `board.js`.
- No suprimas el validador de PWA si detecta que el nuevo módulo falta en `APP_SHELL`.
- No agregues una dependencia de terceros para resolver el checkpoint.

## Qué deberías observar

Si importas `runtime-status.js` desde `app.js` y olvidas cachearlo, `npm run check` debe fallar. Esa falla es intencional: el contrato offline debe acompañar la evolución del grafo de módulos.

## Preguntas de reflexión

- ¿Por qué `navigator.onLine` no demuestra que tu API o Internet sean alcanzables?
- ¿Por qué agregar un módulo nuevo exige pensar en el app shell?
- ¿Por qué IndexedDB y Cache Storage resuelven problemas distintos?
- ¿Qué deuda aparecería si nunca cambiaras `CACHE_NAME` al modificar recursos precacheados?
