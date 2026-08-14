import assert from "node:assert/strict";
import test from "node:test";
import { createClient } from "../src/domain/clients.js";
import { changeProjectStatus, createProject, parseProjectStatus, queryProjects } from "../src/domain/projects.js";
import { changeQuoteStatus, createQuote, parseQuoteStatus, queryQuotes } from "../src/domain/quotes.js";

test("createClient normaliza nombre y correo", () => {
  const client = createClient("c1", { name: "  Acme Norte  ", email: "VENTAS@EXAMPLE.COM " });
  assert.deepEqual(client, { id: "c1", name: "Acme Norte", email: "ventas@example.com" });
});

test("createClient rechaza un correo sin dominio", () => {
  assert.throws(() => createClient("c1", { name: "Acme", email: "ventas@" }), /correo/i);
});

test("createQuote calcula subtotal e inicia como draft sin mutar los datos", () => {
  const items = [{ description: "Arquitectura", quantity: 2, unitPrice: 1500 }];
  const quote = createQuote("q1", { clientId: "c1", items });
  assert.equal(quote.subtotal, 3000);
  assert.equal(quote.status, "draft");
  assert.deepEqual(items, [{ description: "Arquitectura", quantity: 2, unitPrice: 1500 }]);
});

test("createQuote rechaza cantidades no positivas", () => {
  assert.throws(
    () => createQuote("q1", { clientId: "c1", items: [{ description: "Trabajo", quantity: 0, unitPrice: 10 }] }),
    /cantidad/i,
  );
});

test("cotización sólo permite draft → sent → accepted|rejected", () => {
  const draft = createQuote("q1", { clientId: "c1", items: [{ description: "Sitio", quantity: 1, unitPrice: 100 }] });
  assert.throws(() => changeQuoteStatus(draft, "accepted"), /no permitida/i);
  const sent = changeQuoteStatus(draft, "sent");
  const accepted = changeQuoteStatus(sent, "accepted");
  assert.equal(accepted.status, "accepted");
  assert.throws(() => changeQuoteStatus(accepted, "rejected"), /no permitida/i);
});

test("parseQuoteStatus valida valores externos y queryQuotes combina filtros", () => {
  assert.equal(parseQuoteStatus("sent"), "sent");
  assert.throws(() => parseQuoteStatus("expired"), /estado.*inválido/i);
  const draft = createQuote("q1", { clientId: "c1", items: [{ description: "A", quantity: 1, unitPrice: 10 }] });
  const sent = changeQuoteStatus(createQuote("q2", { clientId: "c2", items: [{ description: "B", quantity: 1, unitPrice: 20 }] }), "sent");
  assert.deepEqual(queryQuotes([draft, sent], { status: "sent" }).map((quote) => quote.id), ["q2"]);
  assert.deepEqual(queryQuotes([draft, sent], { clientId: "c1" }).map((quote) => quote.id), ["q1"]);
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

test("parseProjectStatus valida runtime y queryProjects combina filtros", () => {
  assert.equal(parseProjectStatus("active"), "active");
  assert.throws(() => parseProjectStatus("paused"), /estado.*inválido/i);
  const planned = createProject("p1", { clientId: "c1", name: "Portal" });
  const active = changeProjectStatus(createProject("p2", { clientId: "c2", name: "API" }), "active");
  assert.deepEqual(queryProjects([planned, active], { status: "active" }).map((project) => project.id), ["p2"]);
});
