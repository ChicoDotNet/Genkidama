import { createServer } from "node:http";
import { resolve } from "node:path";
import { createAppState, createRequestHandler } from "./app.js";
import { RequestMetrics } from "./diagnostics.js";
import { JsonFileStateStore } from "./persistence.js";

const port = Number(process.env.PORT ?? 3000);
const dataFile = resolve(process.env.FREELANCEDESK_DATA_FILE ?? "data/freelance-desk.json");
const store = new JsonFileStateStore(dataFile);
const state = createAppState(await store.load());
const diagnostics = process.env.FREELANCEDESK_DIAGNOSTICS === "1" ? new RequestMetrics() : undefined;
const server = createServer(createRequestHandler(state, store, diagnostics ? { diagnostics } : {}));

server.listen(port, () => {
  console.log(`FreelanceDesk disponible en http://localhost:${port}`);
  if (diagnostics) console.log("Diagnóstico agregado habilitado en /api/diagnostics.");
});
