import assert from "node:assert/strict";
import test from "node:test";
import {
  addCard,
  cardsInColumn,
  createBoard,
  deleteCard,
  editCard,
  filterCards,
  moveCard,
  normalizeTitle,
} from "../src/board.js";
import { exportBoard, importBoard, loadBoard, saveBoard } from "../src/storage.js";

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

test("edita una tarjeta sin mutar el tablero previo", () => {
  const board = addCard(createBoard(), "Preparar demo", "card-1");
  const edited = editCard(board, "card-1", "  Preparar   demo final ");
  assert.equal(board.cards[0].title, "Preparar demo");
  assert.equal(edited.cards[0].title, "Preparar demo final");
  assert.throws(() => editCard(board, "missing", "Título válido"), /tarjeta no existe/);
});

test("elimina una tarjeta existente sin mutar el tablero previo", () => {
  const board = addCard(createBoard(), "Preparar demo", "card-1");
  const deleted = deleteCard(board, "card-1");
  assert.equal(board.cards.length, 1);
  assert.equal(deleted.cards.length, 0);
  assert.throws(() => deleteCard(board, "missing"), /tarjeta no existe/);
});

test("filtra por columna y texto sin distinguir mayúsculas", () => {
  let board = addCard(createBoard(), "Preparar DEMO", "card-1");
  board = addCard(board, "Escribir documentación", "card-2");
  board = moveCard(board, "card-2", "done");
  assert.deepEqual(filterCards(board, { query: "demo" }).map((card) => card.id), ["card-1"]);
  assert.deepEqual(filterCards(board, { column: "done" }).map((card) => card.id), ["card-2"]);
  assert.equal(filterCards(board, { query: "document", column: "done" }).length, 1);
  assert.throws(() => filterCards(board, { column: "archive" }), /columna de filtro/);
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

test("carga el formato localStorage de las primeras lecciones sin perder datos", () => {
  const legacy = { cards: [{ id: "legacy-1", title: "Tarjeta previa", column: "todo" }] };
  const storage = { getItem: () => JSON.stringify(legacy), setItem() {} };
  assert.deepEqual(loadBoard(storage), legacy);
});

test("exporta e importa un tablero versionado sin perder datos", () => {
  let board = addCard(createBoard(), "Preparar demo", "card-1");
  board = moveCard(board, "card-1", "doing");
  const serialized = exportBoard(board);
  assert.equal(JSON.parse(serialized).version, 1);
  assert.deepEqual(importBoard(serialized), board);
});

test("rechaza importaciones incompatibles o inválidas", () => {
  assert.throws(() => importBoard("{no-json"), /JSON válido/);
  assert.throws(() => importBoard(JSON.stringify({ version: 2, cards: [] })), /version 1/);
  assert.throws(
    () => importBoard(JSON.stringify({ version: 1, cards: [{ id: "1", title: "Válida", column: "archive" }] })),
    /columna desconocida/,
  );
  assert.throws(
    () => importBoard(JSON.stringify({ version: 1, cards: [
      { id: "1", title: "Primera", column: "todo" },
      { id: "1", title: "Segunda", column: "done" },
    ] })),
    /duplicados/,
  );
});
