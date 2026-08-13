import { createServer } from "node:http";
import { extname, isAbsolute, relative, resolve } from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

const root = fileURLToPath(new URL("..", import.meta.url));
const types = {
  ".html": "text/html; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".webmanifest": "application/manifest+json; charset=utf-8",
  ".svg": "image/svg+xml",
};
const securityHeaders = Object.freeze({
  "content-security-policy": "default-src 'self'; base-uri 'none'; form-action 'self'; frame-ancestors 'none'; object-src 'none'",
  "cross-origin-opener-policy": "same-origin",
  "permissions-policy": "camera=(), geolocation=(), microphone=()",
  "referrer-policy": "no-referrer",
  "x-content-type-options": "nosniff",
  "x-frame-options": "DENY",
});
const port = Number(process.env.PORT || 4173);

function resolveRequestPath(pathname) {
  const decoded = decodeURIComponent(pathname);
  const requested = decoded === "/" ? "index.html" : decoded.replace(/^\/+/, "");
  const file = resolve(root, requested);
  const fromRoot = relative(root, file);
  if (fromRoot.startsWith("..") || isAbsolute(fromRoot)) {
    throw new Error("Ruta inválida");
  }
  return file;
}

createServer(async (req, res) => {
  try {
    const pathname = new URL(req.url, `http://${req.headers.host}`).pathname;
    const file = resolveRequestPath(pathname);
    const body = await readFile(file);
    res.writeHead(200, {
      ...securityHeaders,
      "content-type": types[extname(file)] || "application/octet-stream",
    });
    res.end(body);
  } catch {
    res.writeHead(404, {
      ...securityHeaders,
      "content-type": "text/plain; charset=utf-8",
    });
    res.end("Not found");
  }
}).listen(port, "127.0.0.1", () => console.log(`Kanban Local: http://127.0.0.1:${port}`));
