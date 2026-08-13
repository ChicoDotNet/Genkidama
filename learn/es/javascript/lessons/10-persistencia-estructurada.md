# Lección 10 — Persistencia estructurada en el navegador

## Qué vas a conseguir
Usarás una base local asíncrona para conservar el tablero y mantendrás una ruta compatible con el almacenamiento anterior.

## Antes de empezar
Abre `src/idb-storage.js` y `src/repository.js`.

## El problema
La persistencia estructurada del navegador no responde de forma síncrona.

## Concepto
La API IndexedDB organiza bases, almacenes de objetos y transacciones.

## Demostración
[DEMO] Crea tarjetas, recarga y observa la base local desde las herramientas del navegador.

## Código real
`loadPreferredBoard` intenta la nueva persistencia primero y usa el almacenamiento anterior como compatibilidad.

## Qué acaba de pasar
La aplicación evolucionó almacenamiento sin acoplar el dominio a la API del navegador.

## Errores comunes
Mezclar dominio y persistencia; asumir disponibilidad permanente; no planear compatibilidad.

## Buenas prácticas
Valida antes de escribir y prueba la ruta alternativa.

## Tu turno
Añade una prueba del caso donde la fuente primaria no devuelve tablero.

## Cómo comprobar
`npm run check && npm test`

## Solución
Compara con `repository.test.js` después de intentarlo.

## Reto adicional
Diseña almacenes e índices para un tablero mucho mayor.

## Resumen
IndexedDB aporta persistencia estructurada y asíncrona.

## Siguiente paso
La Lección 11 añadirá disponibilidad offline de la app shell.

## Referencias
- [IndexedDB API — MDN](https://developer.mozilla.org/docs/Web/API/IndexedDB_API)
