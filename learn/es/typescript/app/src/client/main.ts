import type { Client, Quote } from "../domain/models.js";

const clientForm = document.querySelector<HTMLFormElement>("#client-form");
const quoteForm = document.querySelector<HTMLFormElement>("#quote-form");
const clientSelect = document.querySelector<HTMLSelectElement>("#quote-client");
const output = document.querySelector<HTMLElement>("#output");

if (!clientForm || !quoteForm || !clientSelect || !output) {
  throw new Error("La interfaz no contiene los elementos requeridos.");
}

async function requestJson<T>(url: string, init?: RequestInit): Promise<T> {
  const response = await fetch(url, init);
  const body = (await response.json()) as T | { error: string };
  if (!response.ok) {
    throw new Error("error" in body ? body.error : `HTTP ${response.status}`);
  }
  return body as T;
}

async function refreshClients(): Promise<void> {
  const clients = await requestJson<Client[]>("/api/clients");
  clientSelect.replaceChildren(
    ...clients.map((client) => {
      const option = document.createElement("option");
      option.value = client.id;
      option.textContent = client.name;
      return option;
    }),
  );
}

clientForm.addEventListener("submit", async (event) => {
  event.preventDefault();
  try {
    const data = new FormData(clientForm);
    const client = await requestJson<Client>("/api/clients", {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ name: data.get("name"), email: data.get("email") }),
    });
    output.textContent = `Cliente creado: ${client.name}`;
    clientForm.reset();
    await refreshClients();
  } catch (error: unknown) {
    output.textContent = error instanceof Error ? error.message : "Error inesperado.";
  }
});

quoteForm.addEventListener("submit", async (event) => {
  event.preventDefault();
  try {
    const data = new FormData(quoteForm);
    const quote = await requestJson<Quote>("/api/quotes", {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        clientId: data.get("clientId"),
        items: [{
          description: data.get("description"),
          quantity: Number(data.get("quantity")),
          unitPrice: Number(data.get("unitPrice")),
        }],
      }),
    });
    output.textContent = `Cotización creada. Subtotal: $${quote.subtotal.toFixed(2)}`;
    quoteForm.reset();
    await refreshClients();
  } catch (error: unknown) {
    output.textContent = error instanceof Error ? error.message : "Error inesperado.";
  }
});

await refreshClients();
