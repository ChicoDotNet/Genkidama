import assert from "node:assert/strict";
import { addCard, createBoard, moveCard } from "../src/board.js";
let board = createBoard();
board = addCard(board, "Preparar propuesta", "smoke-1");
board = moveCard(board, "smoke-1", "done");
assert.equal(board.cards[0].column, "done");
console.log("Kanban Local smoke passed: 1 card moved to done.");
