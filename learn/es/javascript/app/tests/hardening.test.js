import assert from "node:assert/strict";
import test from "node:test";
import { addCard, assertValidBoard, MAX_CARDS } from "../src/board.js";
import { assertImportFileSize, MAX_IMPORT_BYTES } from "../src/storage.js";

function fullBoard(extra = 0) {
  return {
    cards: Array.from({ length: MAX_CARDS + extra }, (_, index) => ({
      id: `card-${index}`,
      title: `Tarjeta ${index}`,
      column: "todo",
    })),
  };
}

test("el dominio no permite crecer más allá del límite de tarjetas", () => {
  assert.throws(() => addCard(fullBoard(), "Una más", "overflow"), /alcanzó el límite/);
});

test("la validación externa rechaza tableros sobredimensionados", () => {
  assert.throws(() => assertValidBoard(fullBoard(1)), /no puede contener más/);
});

test("la frontera de importación acepta el máximo exacto y rechaza exceso", () => {
  assert.equal(assertImportFileSize(MAX_IMPORT_BYTES), MAX_IMPORT_BYTES);
  assert.throws(() => assertImportFileSize(MAX_IMPORT_BYTES + 1), /supera el límite/);
});

test("la frontera de importación rechaza tamaños imposibles", () => {
  assert.throws(() => assertImportFileSize(-1), /entero no negativo/);
  assert.throws(() => assertImportFileSize(Number.NaN), /entero no negativo/);
});
