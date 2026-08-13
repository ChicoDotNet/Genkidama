# Solución de referencia — Checkpoint 03

Una solución válida mantiene el diagnóstico como lógica pura y deja `navigator`/service worker en la frontera del navegador.

## Estado de ejecución

```js
const PERSISTENCE_LABELS = Object.freeze({
  indexeddb: "IndexedDB",
  localstorage: "localStorage",
});

/** Describe runtime state without reading browser globals. */
export function formatRuntimeStatus({ persistenceMode, offlineReady, online }) {
  const persistence = PERSISTENCE_LABELS[persistenceMode];
  if (!persistence) {
    throw new RangeError("Modo de persistencia desconocido.");
  }

  const shell = offlineReady
    ? "app shell lista para uso offline"
    : "app shell todavía no confirmada";
  const network = online
    ? "el navegador reporta estado de red en línea"
    : "el navegador reporta estado de red sin conexión";

  return `Persistencia: ${persistence}; ${shell}; ${network}.`;
}
```

La función no necesita `window` ni `navigator`; por eso puede probarse con Node.

## Pruebas

```js
import assert from "node:assert/strict";
import test from "node:test";
import { formatRuntimeStatus } from "../src/runtime-status.js";

test("describe IndexedDB con shell offline lista", () => {
  const text = formatRuntimeStatus({
    persistenceMode: "indexeddb",
    offlineReady: true,
    online: true,
  });

  assert.match(text, /IndexedDB/);
  assert.match(text, /lista para uso offline/);
  assert.match(text, /navegador reporta.*en línea/);
});

test("rechaza un modo de persistencia desconocido", () => {
  assert.throws(
    () => formatRuntimeStatus({ persistenceMode: "memory", offlineReady: false, online: false }),
    /desconocido/,
  );
});
```

## Integración en la UI

Una forma sencilla es conservar el estado de plataforma fuera del dominio:

```js
let offlineReady = false;

function refreshRuntimeStatus() {
  setStatus(formatRuntimeStatus({
    persistenceMode,
    offlineReady,
    online: navigator.onLine,
  }));
}

window.addEventListener("online", refreshRuntimeStatus);
window.addEventListener("offline", refreshRuntimeStatus);
```

Después de registrar el service worker:

```js
await navigator.serviceWorker.register("./service-worker.js");
await navigator.serviceWorker.ready;
offlineReady = true;
refreshRuntimeStatus();
```

`navigator.onLine` sólo se presenta como lo que el navegador reporta. No sustituye un health check ni demuestra que un servicio concreto sea alcanzable.

## Evolución del app shell

Al importar un archivo nuevo desde `app.js`, la cache debe acompañar el grafo de módulos:

```js
const CACHE_NAME = "kanban-local-v2";
const APP_SHELL = [
  // ...recursos existentes...
  "./src/runtime-status.js",
];
```

El validador PWA recorre imports locales desde `./src/app.js`; por eso `npm run check` falla si el módulo nuevo no está en `APP_SHELL`.

## Qué revisar en tu solución

- estado operativo separado del dominio;
- ninguna afirmación falsa sobre conectividad;
- service worker listo antes de declarar capacidad offline;
- pruebas deterministas sin navegador;
- cache versionada al cambiar el shell;
- módulo nuevo incluido en la app shell;
- ninguna dependencia externa añadida sólo para completar el ejercicio.
