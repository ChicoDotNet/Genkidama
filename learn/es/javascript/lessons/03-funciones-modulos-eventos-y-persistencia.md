# Lección 3 — Funciones, módulos, eventos y persistencia

## Qué vas a conseguir
Conectarás reglas puras con formularios/clicks y conservarás el tablero en `localStorage` sin acoplar toda la aplicación al navegador.

## Antes de empezar
Abre `src/app.js`, `src/storage.js` y las DevTools en la vista de almacenamiento.

## El problema
Una función de dominio no debería saber qué botón se pulsó ni dónde guarda datos el navegador. Si lo sabe, cada prueba necesita reproducir toda la UI.

## Concepto
Los módulos separan responsabilidades mediante `export`/`import`. Los eventos informan qué ocurrió. `preventDefault()` evita que el formulario recargue la página. La persistencia queda detrás de `loadBoard(storage)` y `saveBoard(storage, board)`.

## Demostración
[DEMO] Agrega una tarjeta, recarga y verifica que permanece. Localiza la clave `genkidama-kanban-v1` en DevTools.

## Código real
`app.js` mantiene `board`, llama a `addCard`/`moveCard`, guarda y renderiza. `storage.js` sólo necesita un objeto compatible con `getItem`/`setItem`, así que puede probarse con memoria.

## Qué acaba de pasar
La UI controla efectos; el dominio decide reglas; storage serializa. Más adelante podremos cambiar a IndexedDB sin reescribir reglas.

## Errores comunes
- guardar nodos DOM en `localStorage`;
- confiar en JSON almacenado sin manejar corrupción;
- añadir un listener por tarjeta después de cada render.

## Buenas prácticas
Usa delegación de eventos en el tablero y trata storage como una frontera que puede contener datos antiguos o corruptos.

## Tu turno
Agrega un `console.info` temporal al guardar para observar cuándo ocurre persistencia. Luego retíralo y explica por qué una prueba del dominio no debe depender de ese log.

## Cómo comprobar
`npm test` incluye un round-trip de storage y una prueba de datos corruptos.

## Solución
Compara `src/storage.js` con `tests/board.test.js` después del intento.

## Reto adicional
¿Qué cambiaría si `saveBoard` fuera asíncrona?

## Resumen
- módulos hacen dependencias visibles;
- eventos conectan usuario y estado;
- persistencia es un efecto, no una regla de negocio.

## Siguiente paso
En la [Lección 04](04-pruebas-validacion-y-checkpoint.md) protegeremos comportamiento con pruebas.

## Referencias
- [Events](https://developer.mozilla.org/docs/Learn_web_development/Core/Scripting/Events)
- [`localStorage`](https://developer.mozilla.org/docs/Web/API/Window/localStorage)
