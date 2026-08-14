import assert from "node:assert/strict";
import { createServer } from "node:http";
import test from "node:test";
import { createAppState, createRequestHandler } from "../src/server/app.js";

async function withServer(run: (baseUrl: string) => Promise<void>): Promise<void> {
  const server = createServer(createRequestHandler(createAppState()));
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  try {
    const address = server.address();
    if (!address || typeof address === "string") throw new Error("No se pudo resolver el puerto de prueba.");
    await run(`http://127.0.0.1:${address.port}`);
  } finally {
    await new Promise<void>((resolve, reject) => server.close((error) => error ? reject(error) : resolve()));
  }
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
    const clientResponse = await fetch(`${baseUrl}/api/clients`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ name: "Estudio Uno", email: "hola@example.com" }),
    });
    assert.equal(clientResponse.status, 201);
    const client = await clientResponse.json() as { id: string };

    const quoteResponse = await fetch(`${baseUrl}/api/quotes`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ clientId: client.id, items: [{ description: "Sitio", quantity: 1, unitPrice: 2500 }] }),
    });
    assert.equal(quoteResponse.status, 201);
    const quote = await quoteResponse.json() as { subtotal: number };
    assert.equal(quote.subtotal, 2500);
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
