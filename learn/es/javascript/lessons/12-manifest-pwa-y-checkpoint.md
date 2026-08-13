# Lección 12 — Manifest, PWA y checkpoint 03

## Qué vas a conseguir
Completarás la primera PWA instalable y offline del curso, conectando persistencia local, app shell y metadata de aplicación.

## Antes de empezar
Ejecuta `npm run check`, `npm test` y `npm start`.

## El problema
Una app disponible offline no es automáticamente instalable. El navegador necesita metadata de aplicación.

## Concepto
`manifest.webmanifest` declara nombre, ruta inicial, alcance, modo standalone, colores e icono. `index.html` enlaza el manifest y el service worker incluye esos archivos en la app shell.

## Demostración
[DEMO] Revisa Manifest y Service Workers en DevTools. Instala la app si tu navegador ofrece la opción.

## Código real
Kanban Local separa dominio, persistencia y UI/plataforma. El manifest y el service worker no contienen reglas del tablero.

## Qué acaba de pasar
La PWA se construyó con APIs de plataforma, sin framework ni dependencia externa.

## Errores comunes
Suponer que el manifest por sí solo ofrece offline; usar rutas que rompen al copiar la carpeta; olvidar incluir nuevos módulos en la app shell.

## Buenas prácticas
Mantén rutas relativas y verifica una recarga offline después de una primera carga online.

## Tu turno — Checkpoint 03
Resuelve [`../exercises/checkpoint-03.md`](../exercises/checkpoint-03.md) sin abrir la solución.

[PAUSA PARA EJERCICIO]

## Cómo comprobar
Ejecuta `npm run check`, `npm test`, `npm run smoke` y después prueba online → offline → recarga.

## Solución
Compara con [`../solutions/checkpoint-03.md`](../solutions/checkpoint-03.md) cuando termines.

## Reto adicional
Diseña cómo informarías que existe una nueva versión de la app shell lista para usarse.

## Resumen
La PWA integra asincronía, almacenamiento estructurado, fallback, cache offline y metadata instalable.

## Siguiente paso
La Lección 13 separará aún mejor responsabilidades antes de entrar a tooling y diagnóstico.

## Referencias
- [Web app manifests — MDN](https://developer.mozilla.org/docs/Web/Progressive_web_apps/Manifest)
- [Progressive web apps — MDN](https://developer.mozilla.org/docs/Web/Progressive_web_apps)
