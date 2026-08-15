# Lección 2 — Datos del tablero: objetos, arrays y render

## Qué vas a conseguir
Entenderás cómo representar un tablero con objetos y arrays y derivar la vista sin mezclar datos con elementos HTML.

## Antes de empezar
Ejecuta `npm start` y abre `src/board.js` junto a las DevTools.

## El problema
Si una tarjeta sólo existe como `<li>`, moverla, filtrarla o persistirla obliga a leer el DOM como si fuera base de datos.

## Concepto
El tablero es `{ cards: [] }`. Cada tarjeta tiene `id`, `title` y `column`. `filter` obtiene las tarjetas de una columna y `map` permite producir una colección transformada.

## Demostración
[EN PANTALLA] Revisa `createBoard`, `addCard`, `moveCard` y `cardsInColumn`. Observa que retornan objetos nuevos en lugar de mutar silenciosamente el argumento.

## Código real
`cardsInColumn(board, "doing")` usa `filter`. `moveCard` usa `map` y reemplaza sólo la tarjeta objetivo con `{ ...card, column: targetColumn }`.

## Qué acaba de pasar
El estado puede probarse sin HTML. La UI se vuelve una proyección del modelo, no el lugar donde viven las reglas.

## Errores comunes
- comparar objetos completos cuando basta comparar `id`;
- usar índices como identificadores permanentes;
- mutar `card.column` desde cualquier parte.

## Buenas prácticas
Usa identificadores estables, objetos pequeños y funciones con una regla concreta.

## Tu turno
Crea un tablero, agrega dos tarjetas con ids conocidos y usa `cardsInColumn` para obtener sólo las pendientes.

## Cómo comprobar
Ejecuta `npm test`. Las pruebas verifican que el tablero anterior no se muta al agregar o mover.

## Solución
Revisa `tests/board.test.js` después de intentarlo.

## Reto adicional
¿Por qué conviene calcular el número de terminadas con `filter(...).length` en vez de mantener otro contador?

## Resumen
- objetos modelan entidades;
- arrays modelan colecciones;
- `map` y `filter` transforman y derivan estado claramente.

## Siguiente paso
En la [Lección 03](03-funciones-modulos-eventos-y-persistencia.md) conectaremos reglas con eventos y almacenamiento.

## Referencias
- [Array](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/Array)
