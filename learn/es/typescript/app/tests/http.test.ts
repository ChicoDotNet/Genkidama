import assert from "node:assert/strict";
import { createServer } from "node:http";
import test from "node:test";
import { createAppState, createRequestHandler } from "../src/server/app.js";
import type { AppSnapshot, AppStateStore } from "../src/server/persistence.js";

class CaptureStore implements AppStateStore {
  public readonly saves: AppSnapshot[] = [];
  public failNextSave = false;

  public async load(): Promise<AppSnapshot> {
    return { clients: [], quotes: [], projects: [] };
  }

  public async save(snapshot: AppSnapshot): Promise<void> {
    if (this.failNextSave) {
      this.failNextSave = false;
      throw new Error("disk unavailable");
    }
    this.saves.push(snapshot);
  }
}

async function withServer(run: (baseUrl: string, store: CaptureStore) => Promise<void>): Promise<void> {
  const store = new CaptureStore();
  const server = createServer(createRequestHandler(createAppState(), store));
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  try {
    const address = server.address();
    if (!address || typeof address === "string") throw new Error("No se pudo resolver el puerto de prueba.");
    await run(`http://127.0.0.1:${address.port}`, store);
  } finally {
    await new Promise<void>((resolve, reject) => server.close((error) => error ? reject(error) : resolve()));
  }
}

async function createClient(baseUrl: string): Promise<string> {
  const response = await fetch(`${baseUrl}/api/clients`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ name: "Estudio Uno", email: "hola@example.com" }),
  });
  assert.equal(response.status, 201);
  return (await response.json() as { id: string }).id;
}

async function createQuote(baseUrl: string, clientId: string): Promise<string> {
  const response = await fetch(`${baseUrl}/api/quotes`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ clientId, items: [{ description: "Sitio", quantity: 1, unitPrice: 2500 }] }),
  });
  assert.equal(response.status, 201);
  return (await response.json() as { id: string }).id;
}

test("servidor entrega la interfaz web compilada", async () => {
  await withServer(async (baseUrl) => {
    const response = await fetch(`${baseUrl}/`);
    assert.equal(response.status, 200);
    assert.match(await response.text(), /FreelanceDesk/);
  });
});

test("API crea cliente y cotización conectados", async () => {
  await withServer(async (baseUrl) => {
    const clientId = await createClient(baseUrl);
    const quoteResponse = await fetch(`${baseUrl}/api/quotes`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ clientId, items: [{ description: "Sitio", quantity: 1, unitPrice: 2500 }] }),
    });
    assert.equal(quoteResponse.status, 201);
    const quote = await quoteResponse.json() as { subtotal: number; status: string };
    assert.equal(quote.subtotal, 2500);
    assert.equal(quote.status, "draft");
  });
});

test("API no crea cotización para cliente inexistente", async () => {
  await withServer(async (baseUrl) => {
    const response = await fetch(`${baseUrl}/api/quotes`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ clientId: "missing", items: [{ description: "Sitio", quantity: 1, unitPrice: 10 }] }),
    });
    assert.equal(response.status, 400);
    assert.match(await response.text(), /no existe/i);
  });
});

test("API filtra proyectos por estado y rechaza filtros desconocidos", async () => {
  await withServer(async (baseUrl) => {
    const clientId = await createClient(baseUrl);
    const created = await fetch(`${baseUrl}/api/projects`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ clientId, name: "Portal B2B" }),
    });
    const project = await created.json() as { id: string };
    await fetch(`${baseUrl}/api/projects/${project.id}/status`, {
      method: "PATCH",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ status: "active" }),
    });

    const active = await fetch(`${baseUrl}/api/projects?status=active`);
    assert.equal(active.status, 200);
    assert.equal((await active.json() as unknown[]).length, 1);

    const invalid = await fetch(`${baseUrl}/api/projects?status=paused`);
    assert.equal(invalid.status, 400);
    assert.match(await invalid.text(), /estado.*inválido/i);
  });
});

test("API gobierna ciclo comercial y consulta cotizaciones", async () => {
  await withServer(async (baseUrl) => {
    const clientId = await createClient(baseUrl);
    const quoteId = await createQuote(baseUrl, clientId);

    const invalid = await fetch(`${baseUrl}/api/quotes/${quoteId}/status`, {
      method: "PATCH",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ status: "accepted" }),
    });
    assert.equal(invalid.status, 400);

    const sent = await fetch(`${baseUrl}/api/quotes/${quoteId}/status`, {
      method: "PATCH",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ status: "sent" }),
    });
    assert.equal(sent.status, 200);

    const filtered = await fetch(`${baseUrl}/api/quotes?clientId=${encodeURIComponent(clientId)}&status=sent`);
    assert.equal(filtered.status, 200);
    const quotes = await filtered.json() as Array<{ id: string }>;
    assert.deepEqual(quotes.map((quote) => quote.id), [quoteId]);
  });
});

test("API crea proyecto y sólo permite transiciones secuenciales", async () => {
  await withServer(async (baseUrl, store) => {
    const clientId = await createClient(baseUrl);
    const projectResponse = await fetch(`${baseUrl}/api/projects`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ clientId, name: "Portal B2B" }),
    });
    assert.equal(projectResponse.status, 201);
    const project = await projectResponse.json() as { id: string; status: string };
    assert.equal(project.status, "planned");

    const invalid = await fetch(`${baseUrl}/api/projects/${project.id}/status`, {
      method: "PATCH",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ status: "completed" }),
    });
    assert.equal(invalid.status, 400);
    assert.match(await invalid.text(), /no permitida/i);

    const active = await fetch(`${baseUrl}/api/projects/${project.id}/status`, {
      method: "PATCH",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ status: "active" }),
    });
    assert.equal(active.status, 200);
    assert.equal((await active.json() as { status: string }).status, "active");
    assert.equal(store.saves.at(-1)?.projects[0]?.status, "active");
  });
});

test("API rechaza estado externo desconocido sin persistir el cambio", async () => {
  await withServer(async (baseUrl, store) => {
    const clientId = await createClient(baseUrl);
    const created = await fetch(`${baseUrl}/api/projects`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ clientId, name: "Auditoría" }),
    });
    const project = await created.json() as { id: string };
    const savesBefore = store.saves.length;

    const response = await fetch(`${baseUrl}/api/projects/${project.id}/status`, {
      method: "PATCH",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ status: "paused" }),
    });
    assert.equal(response.status, 400);
    assert.match(await response.text(), /estado.*inválido/i);
    assert.equal(store.saves.length, savesBefore);
  });
});

test("una falla de persistencia responde 503 y no deja memoria adelantada", async () => {
  await withServer(async (baseUrl, store) => {
    const clientId = await createClient(baseUrl);
    const before = await fetch(`${baseUrl}/api/projects`);
    assert.deepEqual(await before.json(), []);

    store.failNextSave = true;
    const response = await fetch(`${baseUrl}/api/projects`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ clientId, name: "No debe quedar" }),
    });
    assert.equal(response.status, 503);
    assert.match(await response.text(), /persistir/i);

    const after = await fetch(`${baseUrl}/api/projects`);
    assert.deepEqual(await after.json(), []);
  });
});
