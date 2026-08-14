import { randomUUID } from "node:crypto";
import { readFile } from "node:fs/promises";
import type { IncomingMessage, ServerResponse } from "node:http";
import { performance } from "node:perf_hooks";
import { fileURLToPath } from "node:url";
import { createClient } from "../domain/clients.js";
import type { Client, CreateClientInput, CreateProjectInput, CreateQuoteInput, Project, Quote } from "../domain/models.js";
import { changeProjectStatus, createProject, parseProjectStatus, queryProjects } from "../domain/projects.js";
import { changeQuoteStatus, createQuote, parseQuoteStatus, queryQuotes } from "../domain/quotes.js";
import type { RequestMetrics } from "./diagnostics.js";
import type { AppSnapshot, AppStateStore } from "./persistence.js";

const DEFAULT_MAX_JSON_BYTES = 64 * 1024;

/** Estado mutable en memoria; la persistencia se mantiene detrás de `AppStateStore`. */
export interface AppState {
  readonly clients: Client[];
  readonly quotes: Quote[];
  readonly projects: Project[];
}

/** Opciones operativas del adaptador HTTP; no forman parte del dominio. */
export interface RequestHandlerOptions {
  readonly diagnostics?: RequestMetrics;
  readonly maxJsonBytes?: number;
  readonly now?: () => number;
}

class HttpFailure extends Error {
  public constructor(public readonly statusCode: number, message: string) {
    super(message);
  }
}

class PersistenceFailure extends HttpFailure {
  public constructor() {
    super(503, "No se pudo persistir el cambio. Intenta de nuevo.");
  }
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
      throw new PersistenceFailure();
    }
  }
  replaceArray(state.clients, next.clients);
  replaceArray(state.quotes, next.quotes);
  replaceArray(state.projects, next.projects);
}

function requireJsonContentType(request: IncomingMessage): void {
  const contentType = request.headers["content-type"] ?? "";
  if (!contentType.toLowerCase().startsWith("application/json")) {
    throw new HttpFailure(415, "La petición debe usar Content-Type application/json.");
  }
}

async function readJson<T>(request: IncomingMessage, maxBytes: number): Promise<T> {
  requireJsonContentType(request);
  if (!Number.isInteger(maxBytes) || maxBytes <= 0) throw new Error("maxJsonBytes debe ser un entero positivo.");

  const declaredLength = Number(request.headers["content-length"] ?? 0);
  if (Number.isFinite(declaredLength) && declaredLength > maxBytes) {
    request.resume();
    throw new HttpFailure(413, `El cuerpo JSON supera el límite de ${maxBytes} bytes.`);
  }

  const chunks: Buffer[] = [];
  let totalBytes = 0;
  for await (const chunk of request) {
    const buffer = Buffer.from(chunk);
    totalBytes += buffer.length;
    if (totalBytes > maxBytes) {
      request.resume();
      throw new HttpFailure(413, `El cuerpo JSON supera el límite de ${maxBytes} bytes.`);
    }
    chunks.push(buffer);
  }
  if (chunks.length === 0) throw new HttpFailure(400, "La petición requiere un cuerpo JSON.");
  return JSON.parse(Buffer.concat(chunks).toString("utf8")) as T;
}

function applySecurityHeaders(response: ServerResponse): void {
  response.setHeader("x-content-type-options", "nosniff");
  response.setHeader("referrer-policy", "no-referrer");
  response.setHeader("content-security-policy", "default-src 'self'; script-src 'self'; object-src 'none'; base-uri 'none'; frame-ancestors 'none'");
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
 * Los errores de entrada conservan un status explícito; una falla de persistencia se informa como 503 sin mutar memoria.
 * El diagnóstico es opt-in y sólo agrega conteos/duraciones, nunca URLs, cuerpos ni datos personales.
 */
export function createRequestHandler(state: AppState, store?: AppStateStore, options: RequestHandlerOptions = {}) {
  const maxJsonBytes = options.maxJsonBytes ?? DEFAULT_MAX_JSON_BYTES;
  const now = options.now ?? (() => performance.now());

  return async (request: IncomingMessage, response: ServerResponse): Promise<void> => {
    const startedAt = now();
    applySecurityHeaders(response);
    try {
      const method = request.method ?? "GET";
      const url = new URL(request.url ?? "/", "http://localhost");

      if (method === "GET" && url.pathname === "/api/diagnostics" && options.diagnostics) {
        sendJson(response, 200, options.diagnostics.snapshot());
        return;
      }
      if (method === "GET" && url.pathname === "/api/clients") {
        sendJson(response, 200, state.clients);
        return;
      }
      if (method === "GET" && url.pathname === "/api/quotes") {
        const rawStatus = url.searchParams.get("status");
        const rawClientId = url.searchParams.get("clientId");
        const query = {
          ...(rawClientId === null ? {} : { clientId: rawClientId }),
          ...(rawStatus === null ? {} : { status: parseQuoteStatus(rawStatus) }),
        };
        sendJson(response, 200, queryQuotes(state.quotes, query));
        return;
      }
      if (method === "GET" && url.pathname === "/api/projects") {
        const rawStatus = url.searchParams.get("status");
        const rawClientId = url.searchParams.get("clientId");
        const query = {
          ...(rawClientId === null ? {} : { clientId: rawClientId }),
          ...(rawStatus === null ? {} : { status: parseProjectStatus(rawStatus) }),
        };
        sendJson(response, 200, queryProjects(state.projects, query));
        return;
      }
      if (method === "POST" && url.pathname === "/api/clients") {
        const client = createClient(randomUUID(), await readJson<CreateClientInput>(request, maxJsonBytes));
        const current = snapshotState(state);
        await commitSnapshot(state, store, { ...current, clients: [...current.clients, client] });
        sendJson(response, 201, client);
        return;
      }
      if (method === "POST" && url.pathname === "/api/quotes") {
        const input = await readJson<CreateQuoteInput>(request, maxJsonBytes);
        if (!state.clients.some((client) => client.id === input.clientId)) throw new HttpFailure(400, "El cliente indicado no existe.");
        const quote = createQuote(randomUUID(), input);
        const current = snapshotState(state);
        await commitSnapshot(state, store, { ...current, quotes: [...current.quotes, quote] });
        sendJson(response, 201, quote);
        return;
      }
      if (method === "POST" && url.pathname === "/api/projects") {
        const input = await readJson<CreateProjectInput>(request, maxJsonBytes);
        if (!state.clients.some((client) => client.id === input.clientId)) throw new HttpFailure(400, "El cliente indicado no existe.");
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
        if (index < 0 || !currentProject) throw new HttpFailure(400, "El proyecto indicado no existe.");
        const input = await readJson<{ readonly status?: unknown }>(request, maxJsonBytes);
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
        if (index < 0 || !currentQuote) throw new HttpFailure(400, "La cotización indicada no existe.");
        const input = await readJson<{ readonly status?: unknown }>(request, maxJsonBytes);
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
      if (error instanceof HttpFailure) {
        sendJson(response, error.statusCode, { error: error.message });
        return;
      }
      const message = error instanceof Error ? error.message : "Error inesperado.";
      sendJson(response, 400, { error: message });
    } finally {
      options.diagnostics?.record(response.statusCode, Math.max(0, now() - startedAt));
    }
  };
}
