import assert from "node:assert/strict";
import { createServer } from "node:http";
import test from "node:test";
import { createAppState, createRequestHandler } from "../src/server/app.js";
import { RequestMetrics } from "../src/server/diagnostics.js";
import type { AppSnapshot, AppStateStore } from "../src/server/persistence.js";

class MemoryStore implements AppStateStore {
  public readonly saves: AppSnapshot[] = [];

  public async load(): Promise<AppSnapshot> {
    return { clients: [], quotes: [], projects: [] };
  }

  public async save(snapshot: AppSnapshot): Promise<void> {
    this.saves.push(snapshot);
  }
}

async function withOperationalServer(
  run: (baseUrl: string, store: MemoryStore) => Promise<void>,
  options: Parameters<typeof createRequestHandler>[2] = {},
): Promise<void> {
  const store = new MemoryStore();
  const server = createServer(createRequestHandler(createAppState(), store, options));
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  try {
    const address = server.address();
    if (!address || typeof address === "string") throw new Error("No se pudo resolver el puerto de prueba.");
    await run(`http://127.0.0.1:${address.port}`, store);
  } finally {
    await new Promise<void>((resolve, reject) => server.close((error) => error ? reject(error) : resolve()));
  }
}

test("respuestas incluyen headers defensivos sin depender del contenido", async () => {
  await withOperationalServer(async (baseUrl) => {
    const response = await fetch(`${baseUrl}/`);
    assert.equal(response.status, 200);
    assert.equal(response.headers.get("x-content-type-options"), "nosniff");
    assert.equal(response.headers.get("referrer-policy"), "no-referrer");
    assert.match(response.headers.get("content-security-policy") ?? "", /default-src 'self'/);
  });
});

test("mutación rechaza media type no JSON sin tocar persistencia", async () => {
  await withOperationalServer(async (baseUrl, store) => {
    const response = await fetch(`${baseUrl}/api/clients`, {
      method: "POST",
      headers: { "content-type": "text/plain" },
      body: JSON.stringify({ name: "No debe entrar", email: "nadie@example.com" }),
    });
    assert.equal(response.status, 415);
    assert.match(await response.text(), /application\/json/i);
    assert.equal(store.saves.length, 0);
  });
});

test("cuerpo mayor al límite responde 413 sin mutar estado", async () => {
  await withOperationalServer(async (baseUrl, store) => {
    const oversized = JSON.stringify({ name: "x".repeat(300), email: "nadie@example.com" });
    const response = await fetch(`${baseUrl}/api/clients`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: oversized,
    });
    assert.equal(response.status, 413);
    assert.match(await response.text(), /límite/i);
    assert.equal(store.saves.length, 0);
  }, { maxJsonBytes: 128 });
});

test("diagnóstico es opt-in y agrega sólo conteos y duración", async () => {
  const diagnostics = new RequestMetrics();
  const clockValues = [10, 14, 20, 23];
  const now = (): number => clockValues.shift() ?? 23;

  await withOperationalServer(async (baseUrl) => {
    const clients = await fetch(`${baseUrl}/api/clients`);
    assert.equal(clients.status, 200);

    const response = await fetch(`${baseUrl}/api/diagnostics`);
    assert.equal(response.status, 200);
    assert.deepEqual(await response.json(), {
      totalRequests: 1,
      failedRequests: 0,
      totalDurationMs: 4,
      maxDurationMs: 4,
    });
  }, { diagnostics, now });

  assert.deepEqual(diagnostics.snapshot(), {
    totalRequests: 2,
    failedRequests: 0,
    totalDurationMs: 7,
    maxDurationMs: 4,
  });
});

test("endpoint de diagnóstico no existe cuando no fue habilitado", async () => {
  await withOperationalServer(async (baseUrl) => {
    const response = await fetch(`${baseUrl}/api/diagnostics`);
    assert.equal(response.status, 404);
  });
});
