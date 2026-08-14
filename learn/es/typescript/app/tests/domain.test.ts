import assert from "node:assert/strict";
import test from "node:test";
import { createClient } from "../src/domain/clients.js";
import { changeProjectStatus, createProject, parseProjectStatus } from "../src/domain/projects.js";
import { createQuote } from "../src/domain/quotes.js";

test("createClient normaliza nombre y correo", () => {
  const client = createClient("c1", { name: "  Acme Norte  ", email: "VENTAS@EXAMPLE.COM " });
  assert.deepEqual(client, { id: "c1", name: "Acme Norte", email: "ventas@example.com" });
});

test("createClient rechaza un correo sin dominio", () => {
  assert.throws(() => createClient("c1", { name: "Acme", email: "ventas@" }), /correo/i);
});

test("createQuote calcula el subtotal sin mutar los datos", () => {
  const items = [{ description: "Arquitectura", quantity: 2, unitPrice: 1500 }];
  const quote = createQuote("q1", { clientId: "c1", items });
  assert.equal(quote.subtotal, 3000);
  assert.deepEqual(items, [{ description: "Arquitectura", quantity: 2, unitPrice: 1500 }]);
});

test("createQuote rechaza cantidades no positivas", () => {
  assert.throws(
    () => createQuote("q1", { clientId: "c1", items: [{ description: "Trabajo", quantity: 0, unitPrice: 10 }] }),
    /cantidad/i,
  );
});

test("createProject normaliza nombre e inicia en planned", () => {
  const project = createProject("p1", { clientId: "c1", name: "  Portal B2B  " });
  assert.deepEqual(project, { id: "p1", clientId: "c1", name: "Portal B2B", status: "planned" });
});

test("changeProjectStatus exige planned → active → completed", () => {
  const planned = createProject("p1", { clientId: "c1", name: "Portal" });
  assert.throws(() => changeProjectStatus(planned, "completed"), /no permitida/i);
  const active = changeProjectStatus(planned, "active");
  const completed = changeProjectStatus(active, "completed");
  assert.equal(completed.status, "completed");
  assert.equal(planned.status, "planned");
});

test("parseProjectStatus valida valores externos en runtime", () => {
  assert.equal(parseProjectStatus("active"), "active");
  assert.throws(() => parseProjectStatus("paused"), /estado.*inválido/i);
});
