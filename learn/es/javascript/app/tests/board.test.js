import assert from "node:assert/strict";
import test from "node:test";
import { addCard, cardsInColumn, createBoard, moveCard, normalizeTitle } from "../src/board.js";
import { loadBoard, saveBoard } from "../src/storage.js";

test("normaliza espacios y valida títulos", () => {
  assert.equal(normalizeTitle("  Preparar   demo  "), "Preparar demo");
  assert.throws(() => normalizeTitle("x"), /entre 3 y 80/);
});

test("agrega una tarjeta nueva en por hacer sin mutar el tablero anterior", () => {
  const empty = createBoard();
  const next = addCard(empty, "Preparar demo", "card-1");
  assert.equal(empty.cards.length, 0);
  assert.deepEqual(next.cards[0], { id: "card-1", title: "Preparar demo", column: "todo" });
});

test("mueve tarjetas entre columnas", () => {
  const board = addCard(createBoard(), "Preparar demo", "card-1");
  const moved = moveCard(board, "card-1", "doing");
  assert.equal(cardsInColumn(moved, "doing").length, 1);
  assert.equal(cardsInColumn(board, "todo").length, 1);
});

test("rechaza columna o tarjeta inexistente", () => {
  const board = addCard(createBoard(), "Preparar demo", "card-1");
  assert.throws(() => moveCard(board, "card-1", "archive"), /columna destino/);
  assert.throws(() => moveCard(board, "missing", "done"), /tarjeta no existe/);
});

test("persiste y recupera el tablero mediante una interfaz compatible con localStorage", () => {
  const memory = new Map();
  const storage = {
    getItem: (key) => memory.get(key) ?? null,
    setItem: (key, value) => memory.set(key, value),
  };
  const board = addCard(createBoard(), "Persistir tarjeta", "card-1");
  saveBoard(storage, board);
  assert.deepEqual(loadBoard(storage), board);
});

test("datos corruptos en storage degradan a un tablero vacío", () => {
  const storage = { getItem: () => "{no-json", setItem() {} };
  assert.deepEqual(loadBoard(storage), { cards: [] });
});
