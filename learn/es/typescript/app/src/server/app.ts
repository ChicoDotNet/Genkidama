import { randomUUID } from "node:crypto";
import { readFile } from "node:fs/promises";
import type { IncomingMessage, ServerResponse } from "node:http";
import { fileURLToPath } from "node:url";
import { createClient } from "../domain/clients.js";
import type { Client, CreateClientInput, CreateProjectInput, CreateQuoteInput, Project, Quote } from "../domain/models.js";
import { changeProjectStatus, createProject, parseProjectStatus, queryProjects } from "../domain/projects.js";
import { changeQuoteStatus, createQuote, parseQuoteStatus, queryQuotes } from "../domain/quotes.js";
import type { AppSnapshot, AppStateStore } from "./persistence.js";

/** Estado mutable en memoria; la persistencia se mantiene detrás de `AppStateStore`. */
export interface AppState {
  readonly clients: Client[];
  readonly quotes: Quote[];
  readonly projects: Project[];
}

class PersistenceFailure extends Error {}

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
  return Object.freeze({ clients: [...state.clients], quotes: [...state.quotes], projects: [...state.projects] });
}

function replaceArray<T>(target: T[], values: readonly T[]): void {
  target.splice(0, target.length, ...values);
}

async function commitSnapshot(state: AppState, store: AppStateStore | undefined, next: AppSnapshot): Promise<void> {
  if (store) {
    try {
      await store.save(next);
    } catch {
      throw new PersistenceFailure("No se pudo persistir el cambio. Intenta de nuevo.");
    }
  }
  replaceArray(state.clients, next.clients);
  replaceArray(state.quotes, next.quotes);
  replaceArray(state.projects, next.projects);
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
 * Los errores de entrada se convierten en 400; una falla de persistencia se informa como 503 sin mutar memoria.
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
        const rawStatus = url.searchParams.get("status");
        const status = rawStatus === null ? undefined : parseQuoteStatus(rawStatus);
        sendJson(response, 200, queryQuotes(state.quotes, { clientId: url.searchParams.get("clientId") ?? undefined, status }));
        return;
      }
      if (method === "GET" && url.pathname === "/api/projects") {
        const rawStatus = url.searchParams.get("status");
        const status = rawStatus === null ? undefined : parseProjectStatus(rawStatus);
        sendJson(response, 200, queryProjects(state.projects, { clientId: url.searchParams.get("clientId") ?? undefined, status }));
        return;
      }
      if (method === "POST" && url.pathname === "/api/clients") {
        const client = createClient(randomUUID(), await readJson<CreateClientInput>(request));
        const current = snapshotState(state);
        await commitSnapshot(state, store, { ...current, clients: [...current.clients, client] });
        sendJson(response, 201, client);
        return;
      }
      if (method === "POST" && url.pathname === "/api/quotes") {
        const input = await readJson<CreateQuoteInput>(request);
        if (!state.clients.some((client) => client.id === input.clientId)) throw new Error("El cliente indicado no existe.");
        const quote = createQuote(randomUUID(), input);
        const current = snapshotState(state);
        await commitSnapshot(state, store, { ...current, quotes: [...current.quotes, quote] });
        sendJson(response, 201, quote);
        return;
      }
      if (method === "POST" && url.pathname === "/api/projects") {
        const input = await readJson<CreateProjectInput>(request);
        if (!state.clients.some((client) => client.id === input.clientId)) throw new Error("El cliente indicado no existe.");
        const project = createProject(randomUUID(), input);
        const current = snapshotState(state);
        await commitSnapshot(state, store, { ...current, projects: [...current.projects, project] });
        sendJson(response, 201, project);
        return;
      }

      const projectStatusMatch = /^\/api\/projects\/([^/]+)\/status$/.exec(url.pathname);
      if (method === "PATCH" && projectStatusMatch) {
        const projectId = decodeURIComponent(projectStatusMatch[1] ?? "");
        const index = state.projects.findIndex((project) => project.id === projectId);
        const currentProject = state.projects[index];
        if (index < 0 || !currentProject) throw new Error("El proyecto indicado no existe.");
        const input = await readJson<{ readonly status?: unknown }>(request);
        const updated = changeProjectStatus(currentProject, parseProjectStatus(input.status));
        const current = snapshotState(state);
        const projects = [...current.projects];
        projects[index] = updated;
        await commitSnapshot(state, store, { ...current, projects });
        sendJson(response, 200, updated);
        return;
      }

      const quoteStatusMatch = /^\/api\/quotes\/([^/]+)\/status$/.exec(url.pathname);
      if (method === "PATCH" && quoteStatusMatch) {
        const quoteId = decodeURIComponent(quoteStatusMatch[1] ?? "");
        const index = state.quotes.findIndex((quote) => quote.id === quoteId);
        const currentQuote = state.quotes[index];
        if (index < 0 || !currentQuote) throw new Error("La cotización indicada no existe.");
        const input = await readJson<{ readonly status?: unknown }>(request);
        const updated = changeQuoteStatus(currentQuote, parseQuoteStatus(input.status));
        const current = snapshotState(state);
        const quotes = [...current.quotes];
        quotes[index] = updated;
        await commitSnapshot(state, store, { ...current, quotes });
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
      if (error instanceof PersistenceFailure) {
        sendJson(response, 503, { error: error.message });
        return;
      }
      const message = error instanceof Error ? error.message : "Error inesperado.";
      sendJson(response, 400, { error: message });
    }
  };
}
