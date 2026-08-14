import { randomUUID } from "node:crypto";
import { readFile } from "node:fs/promises";
import type { IncomingMessage, ServerResponse } from "node:http";
import { fileURLToPath } from "node:url";
import { createClient } from "../domain/clients.js";
import type { Client, CreateClientInput, CreateProjectInput, CreateQuoteInput, Project, Quote } from "../domain/models.js";
import { changeProjectStatus, createProject, parseProjectStatus } from "../domain/projects.js";
import { createQuote } from "../domain/quotes.js";
import type { AppSnapshot, AppStateStore } from "./persistence.js";

/** Estado mutable en memoria; la persistencia se mantiene detrás de `AppStateStore`. */
export interface AppState {
  readonly clients: Client[];
  readonly quotes: Quote[];
  readonly projects: Project[];
}

/** Crea un estado independiente para servidor o pruebas a partir de un snapshot opcional. */
export function createAppState(snapshot?: AppSnapshot): AppState {
  return {
    clients: [...(snapshot?.clients ?? [])],
    quotes: [...(snapshot?.quotes ?? [])],
    projects: [...(snapshot?.projects ?? [])],
  };
}

/** Obtiene una copia serializable del estado actual. */
export function snapshotState(state: AppState): AppSnapshot {
  return Object.freeze({
    clients: [...state.clients],
    quotes: [...state.quotes],
    projects: [...state.projects],
  });
}

async function persist(store: AppStateStore | undefined, state: AppState): Promise<void> {
  if (store) await store.save(snapshotState(state));
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
 * Crea el manejador HTTP de FreelanceDesk sobre estado y persistencia inyectados.
 * Los errores de entrada se convierten en respuestas 400 sin ocultar el mensaje útil.
 */
export function createRequestHandler(state: AppState, store?: AppStateStore) {
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
      if (method === "GET" && url.pathname === "/api/projects") {
        sendJson(response, 200, state.projects);
        return;
      }
      if (method === "POST" && url.pathname === "/api/clients") {
        const client = createClient(randomUUID(), await readJson<CreateClientInput>(request));
        state.clients.push(client);
        await persist(store, state);
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
        await persist(store, state);
        sendJson(response, 201, quote);
        return;
      }
      if (method === "POST" && url.pathname === "/api/projects") {
        const input = await readJson<CreateProjectInput>(request);
        if (!state.clients.some((client) => client.id === input.clientId)) {
          throw new Error("El cliente indicado no existe.");
        }
        const project = createProject(randomUUID(), input);
        state.projects.push(project);
        await persist(store, state);
        sendJson(response, 201, project);
        return;
      }

      const statusMatch = /^\/api\/projects\/([^/]+)\/status$/.exec(url.pathname);
      if (method === "PATCH" && statusMatch) {
        const projectId = decodeURIComponent(statusMatch[1] ?? "");
        const index = state.projects.findIndex((project) => project.id === projectId);
        const current = state.projects[index];
        if (index < 0 || !current) throw new Error("El proyecto indicado no existe.");
        const input = await readJson<{ readonly status?: unknown }>(request);
        const updated = changeProjectStatus(current, parseProjectStatus(input.status));
        state.projects[index] = updated;
        await persist(store, state);
        sendJson(response, 200, updated);
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
