import { randomUUID } from "node:crypto";
import { readFile } from "node:fs/promises";
import type { IncomingMessage, ServerResponse } from "node:http";
import { fileURLToPath } from "node:url";
import { createClient } from "../domain/clients.js";
import type { Client, CreateClientInput, CreateQuoteInput, Quote } from "../domain/models.js";
import { createQuote } from "../domain/quotes.js";

/** Estado en memoria. La persistencia se incorpora en incrementos posteriores. */
export interface AppState {
  readonly clients: Client[];
  readonly quotes: Quote[];
}

/** Crea un estado independiente para servidor o pruebas. */
export function createAppState(): AppState {
  return { clients: [], quotes: [] };
}

async function readJson<T>(request: IncomingMessage): Promise<T> {
  const chunks: Buffer[] = [];
  for await (const chunk of request) chunks.push(Buffer.from(chunk));
  if (chunks.length === 0) throw new Error("La petición requiere un cuerpo JSON.");
  return JSON.parse(Buffer.concat(chunks).toString("utf8")) as T;
}

function sendJson(response: ServerResponse, status: number, value: unknown): void {
  response.writeHead(status, { "content-type": "application/json; charset=utf-8" });
  response.end(JSON.stringify(value));
}

async function sendFile(response: ServerResponse, file: URL, contentType: string): Promise<void> {
  const data = await readFile(fileURLToPath(file));
  response.writeHead(200, { "content-type": contentType });
  response.end(data);
}

/**
 * Crea el manejador HTTP de FreelanceDesk sobre un estado inyectado.
 * Los errores de entrada se convierten en respuestas 400 sin ocultar el mensaje útil.
 */
export function createRequestHandler(state: AppState) {
  return async (request: IncomingMessage, response: ServerResponse): Promise<void> => {
    try {
      const method = request.method ?? "GET";
      const url = new URL(request.url ?? "/", "http://localhost");

      if (method === "GET" && url.pathname === "/api/clients") {
        sendJson(response, 200, state.clients);
        return;
      }
      if (method === "GET" && url.pathname === "/api/quotes") {
        sendJson(response, 200, state.quotes);
        return;
      }
      if (method === "POST" && url.pathname === "/api/clients") {
        const client = createClient(randomUUID(), await readJson<CreateClientInput>(request));
        state.clients.push(client);
        sendJson(response, 201, client);
        return;
      }
      if (method === "POST" && url.pathname === "/api/quotes") {
        const input = await readJson<CreateQuoteInput>(request);
        if (!state.clients.some((client) => client.id === input.clientId)) {
          throw new Error("El cliente indicado no existe.");
        }
        const quote = createQuote(randomUUID(), input);
        state.quotes.push(quote);
        sendJson(response, 201, quote);
        return;
      }
      if (method === "GET" && url.pathname === "/") {
        await sendFile(response, new URL("../../../public/index.html", import.meta.url), "text/html; charset=utf-8");
        return;
      }
      if (method === "GET" && url.pathname === "/assets/main.js") {
        await sendFile(response, new URL("../client/main.js", import.meta.url), "text/javascript; charset=utf-8");
        return;
      }

      sendJson(response, 404, { error: "Ruta no encontrada." });
    } catch (error: unknown) {
      const message = error instanceof Error ? error.message : "Error inesperado.";
      sendJson(response, 400, { error: message });
    }
  };
}
