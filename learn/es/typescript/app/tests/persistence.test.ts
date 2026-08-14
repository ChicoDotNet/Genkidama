import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { JsonFileStateStore, parseSnapshot } from "../src/server/persistence.js";

const snapshot = {
  clients: [{ id: "c1", name: "Acme", email: "hola@example.com" }],
  quotes: [{ id: "q1", clientId: "c1", items: [{ description: "API", quantity: 1, unitPrice: 2500 }], subtotal: 2500, status: "sent" as const }],
  projects: [{ id: "p1", clientId: "c1", name: "Portal", status: "active" as const }],
};

test("JsonFileStateStore persiste y recupera un snapshot completo", async () => {
  const directory = await mkdtemp(join(tmpdir(), "freelance-desk-"));
  const file = join(directory, "state.json");
  try {
    const store = new JsonFileStateStore(file);
    await store.save(snapshot);
    assert.deepEqual(await store.load(), snapshot);
    const text = await readFile(file, "utf8");
    assert.match(text, /"projects"/);
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});

test("persistencia legacy sin estado de cotización migra a draft", () => {
  const parsed = parseSnapshot({
    clients: [],
    quotes: [{ id: "q-old", clientId: "c1", items: [{ description: "API", quantity: 1, unitPrice: 10 }], subtotal: 10 }],
    projects: [],
  });
  assert.equal(parsed.quotes[0]?.status, "draft");
});

test("persistencia rechaza estado de cotización desconocido", () => {
  assert.throws(() => parseSnapshot({
    clients: [],
    quotes: [{ id: "q1", clientId: "c1", items: [{ description: "API", quantity: 1, unitPrice: 10 }], subtotal: 10, status: "expired" }],
    projects: [],
  }), /estado de cotización/i);
});

test("JsonFileStateStore interpreta archivo inexistente como estado vacío", async () => {
  const directory = await mkdtemp(join(tmpdir(), "freelance-desk-empty-"));
  try {
    const store = new JsonFileStateStore(join(directory, "missing.json"));
    assert.deepEqual(await store.load(), { clients: [], quotes: [], projects: [] });
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});

test("persistencia corrupta falla explícitamente en vez de inventar tipos", async () => {
  const directory = await mkdtemp(join(tmpdir(), "freelance-desk-invalid-"));
  const file = join(directory, "state.json");
  try {
    await writeFile(file, JSON.stringify({ clients: [], quotes: [], projects: [{ status: "paused" }] }), "utf8");
    const store = new JsonFileStateStore(file);
    await assert.rejects(() => store.load(), /proyecto mal formado/i);
    assert.throws(() => parseSnapshot({ clients: [], quotes: [] }), /projects/i);
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});
