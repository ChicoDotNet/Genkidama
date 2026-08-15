import assert from "node:assert/strict";
import test from "node:test";
import { createBoard } from "../src/board.js";
import { applyBoardCommand } from "../src/commands.js";

test("la capa de comandos agrega y mueve sin conocer el DOM", () => {
  let board = applyBoardCommand(createBoard(), { type: "add", title: "Preparar demo", id: "card-1" });
  board = applyBoardCommand(board, { type: "move", cardId: "card-1", targetColumn: "doing" });
  assert.deepEqual(board.cards, [{ id: "card-1", title: "Preparar demo", column: "doing" }]);
});

test("la capa de comandos edita y elimina usando reglas del dominio", () => {
  let board = applyBoardCommand(createBoard(), { type: "add", title: "Texto anterior", id: "card-1" });
  board = applyBoardCommand(board, { type: "edit", cardId: "card-1", title: "Texto nuevo" });
  assert.equal(board.cards[0].title, "Texto nuevo");
  board = applyBoardCommand(board, { type: "delete", cardId: "card-1" });
  assert.deepEqual(board.cards, []);
});

test("rechaza comandos desconocidos", () => {
  assert.throws(() => applyBoardCommand(createBoard(), { type: "teleport" }), /Comando desconocido/);
});

test("rechaza objetos sin tipo de comando", () => {
  assert.throws(() => applyBoardCommand(createBoard(), {}), /debe declarar un tipo/);
});
