# Solución de referencia — Checkpoint 01

Una solución pequeña calcula primero cuántas tarjetas están en `doing` y rechaza sólo cuando la tarjeta aún no está ahí y el destino ya tiene tres:

```js
if (
  targetColumn === "doing" &&
  board.cards.find((card) => card.id === cardId)?.column !== "doing" &&
  cardsInColumn(board, "doing").length >= 3
) {
  throw new Error("La columna En curso admite como máximo 3 tarjetas.");
}
```

La prueba importante conserva el tablero anterior, provoca el error y verifica que su contenido no cambió. Otra implementación es válida si mantiene los criterios y sigue siendo fácil de explicar.
