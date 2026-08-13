# Lección 9 — Asincronía real: esperar sin congelar la interfaz

## Qué vas a conseguir
Entenderás `Promise`, `async` y `await` mientras Kanban Local cambia de una persistencia síncrona a una frontera asíncrona real.

## Antes de empezar
Ejecuta `npm test` y abre `src/repository.js`.

## El problema
IndexedDB no responde de forma síncrona. La UI no debe anunciar éxito antes de que la persistencia termine.

## Concepto
Una `Promise` representa un resultado futuro. `await` pausa sólo la función `async`; no bloquea el navegador.

## Demostración
[EN PANTALLA] Observa `persistAndRender`: espera `savePreferredBoard`, después renderiza y actualiza la región de estado.

## Código real
`repository.js` define una frontera asíncrona inyectable. El dominio sigue síncrono y determinista; sólo I/O devuelve promesas.

## Qué acaba de pasar
No convertimos todo en `async`: la asincronía se propagó sólo hasta donde existe I/O real.

## Errores comunes
Olvidar `await`; hacer async funciones puras; anunciar éxito antes de persistir; ocultar fallos sin fallback.

## Buenas prácticas
Mantén el dominio independiente de APIs de navegador e inyecta fronteras para probar éxito/error.

## Tu turno
Añade una prueba donde la escritura primaria falle y comprueba que el modo resultante sea `localstorage`.

## Cómo comprobar
`npm run check && npm test`

## Solución
Compara con las pruebas existentes de `repository.test.js` después de intentarlo.

## Reto adicional
Explica event loop frente a ejecución multihilo.

## Resumen
`async/await` coordina I/O; no debe contaminar lógica pura sin motivo.

## Siguiente paso
Continúa con [Lección 10 — IndexedDB](10-indexeddb-y-migracion.md).

## Referencias
- [Using promises — MDN](https://developer.mozilla.org/docs/Web/JavaScript/Guide/Using_promises)
- [async function — MDN](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Statements/async_function)
