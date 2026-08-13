import assert from "node:assert/strict";
import test from "node:test";
import { addCard, createBoard } from "../src/board.js";
import { loadPreferredBoard, savePreferredBoard } from "../src/repository.js";

function memoryStorage(initial = null) {
  const memory = new Map();
  if (initial !== null) memory.set("genkidama-kanban-v1", initial);
  return { getItem: (key) => memory.get(key) ?? null, setItem: (key, value) => memory.set(key, value) };
}

test("prefiere IndexedDB cuando contiene un tablero", async () => {
  const board = addCard(createBoard(), "Desde IndexedDB", "idb-1");
  const result = await loadPreferredBoard({ indexedDbFactory: {}, storage: memoryStorage(), loadIndexedDb: async () => board });
  assert.deepEqual(result, { board, mode: "indexeddb" });
});

test("usa localStorage cuando IndexedDB está vacío o falla", async () => {
  const legacy = { cards: [{ id: "legacy-1", title: "Tarjeta previa", column: "todo" }] };
  const storage = memoryStorage(JSON.stringify(legacy));
  assert.deepEqual(await loadPreferredBoard({ indexedDbFactory: {}, storage, loadIndexedDb: async () => null }), { board: legacy, mode: "localstorage" });
  const failed = await loadPreferredBoard({ indexedDbFactory: {}, storage, loadIndexedDb: async () => { throw new Error("blocked"); } });
  assert.deepEqual(failed.board, legacy);
});

test("guarda en IndexedDB y conserva copia compatible en localStorage", async () => {
  const storage = memoryStorage();
  const board = addCard(createBoard(), "Persistencia dual", "card-1");
  let captured;
  const mode = await savePreferredBoard({ indexedDbFactory: {}, storage, board, saveIndexedDb: async (_factory, value) => { captured = value; } });
  assert.equal(mode, "indexeddb");
  assert.deepEqual(captured, board);
  assert.equal(JSON.parse(storage.getItem("genkidama-kanban-v1")).version, 1);
});

test("degrada a localStorage si IndexedDB rechaza la escritura", async () => {
  const storage = memoryStorage();
  const board = addCard(createBoard(), "Fallback seguro", "card-1");
  const mode = await savePreferredBoard({ indexedDbFactory: {}, storage, board, saveIndexedDb: async () => { throw new Error("quota"); } });
  assert.equal(mode, "localstorage");
  assert.equal(JSON.parse(storage.getItem("genkidama-kanban-v1")).cards[0].title, "Fallback seguro");
});
