# Solución de referencia — Checkpoint 02

Una solución válida mantiene la regla pura y deja confirmación/persistencia en la frontera del navegador.

## Dominio

```js
/** Remove completed cards without mutating the input board. */
export function clearDone(board) {
  return {
    ...board,
    cards: board.cards.filter((card) => card.column !== "done"),
  };
}
```

No necesita `document`, `window`, `confirm` ni `localStorage`.

## Prueba principal

```js
test("limpia terminadas sin mutar el tablero anterior", () => {
  let board = addCard(createBoard(), "Pendiente", "card-1");
  board = addCard(board, "Terminada", "card-2");
  board = moveCard(board, "card-2", "done");

  const cleaned = clearDone(board);

  assert.deepEqual(cleaned.cards.map((card) => card.id), ["card-1"]);
  assert.equal(board.cards.length, 2);
  assert.deepEqual(importBoard(exportBoard(cleaned)), cleaned);
});
```

## UI

Una frontera posible:

```js
clearDoneButton.addEventListener("click", () => {
  const completed = board.cards.filter((card) => card.column === "done").length;
  if (completed === 0) {
    setStatus("No hay tarjetas terminadas por limpiar.");
    return;
  }

  if (!window.confirm(`¿Eliminar ${completed} tarjeta(s) terminada(s)?`)) return;
  board = clearDone(board);
  persistAndRender(`${completed} tarjeta(s) terminada(s) eliminada(s).`);
});
```

La solución evita confirmación innecesaria si no existe trabajo terminado. Otra decisión puede ser válida si puedes justificarla y preservas el contrato.

## Qué revisar en tu solución

- regla de dominio independiente del navegador;
- no mutación;
- pruebas antes de confiar en la UI;
- confirmación sólo en la frontera;
- persistencia mediante la ruta ya existente;
- feedback accesible;
- ningún atajo que convierta el DOM filtrado en fuente de verdad.
