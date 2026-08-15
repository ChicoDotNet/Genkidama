import assert from "node:assert/strict";
import { access, readFile } from "node:fs/promises";
import { dirname, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

const APP_ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "..");

function toAssetPath(fullPath) {
  const relativePath = relative(APP_ROOT, fullPath).split(sep).join("/");
  return `./${relativePath}`;
}

async function collectLocalModules(assetPath, seen = new Set()) {
  if (seen.has(assetPath)) return seen;
  seen.add(assetPath);

  const fullPath = resolve(APP_ROOT, assetPath.slice(2));
  const source = await readFile(fullPath, "utf8");
  const importPattern = /\b(?:import|export)\s+(?:[^;"']*?\s+from\s+)?["'](\.[^"']+)["']/g;

  for (const match of source.matchAll(importPattern)) {
    const importedPath = resolve(dirname(fullPath), match[1]);
    await collectLocalModules(toAssetPath(importedPath), seen);
  }

  return seen;
}

const [html, manifestText, serviceWorker] = await Promise.all([
  readFile(resolve(APP_ROOT, "index.html"), "utf8"),
  readFile(resolve(APP_ROOT, "manifest.webmanifest"), "utf8"),
  readFile(resolve(APP_ROOT, "service-worker.js"), "utf8"),
]);

const manifest = JSON.parse(manifestText);
assert.equal(manifest.start_url, "./", "El manifest debe conservar start_url relativo.");
assert.equal(manifest.scope, "./", "El manifest debe conservar scope relativo.");
assert.equal(manifest.display, "standalone", "La PWA debe declarar display standalone.");
assert.ok(
  Array.isArray(manifest.icons) && manifest.icons.some((icon) => icon.src === "./icon.svg"),
  "El manifest debe declarar el icono de Kanban Local.",
);

assert.ok(
  html.includes('rel="manifest" href="./manifest.webmanifest"'),
  "index.html debe enlazar manifest.webmanifest.",
);
assert.ok(
  html.includes('type="module" src="./src/app.js"'),
  "index.html debe cargar ./src/app.js como módulo.",
);

const shellMatch = serviceWorker.match(/const APP_SHELL\s*=\s*\[([\s\S]*?)\];/);
assert.ok(shellMatch, "service-worker.js debe declarar APP_SHELL como arreglo literal.");

const appShell = new Set(
  [...shellMatch[1].matchAll(/["']([^"']+)["']/g)].map((match) => match[1]),
);
const localModules = await collectLocalModules("./src/app.js");
const requiredAssets = new Set([
  "./",
  "./index.html",
  "./styles.css",
  "./manifest.webmanifest",
  "./icon.svg",
  ...localModules,
]);

for (const asset of requiredAssets) {
  assert.ok(appShell.has(asset), `APP_SHELL debe incluir ${asset}.`);
}

for (const asset of appShell) {
  if (asset === "./") continue;
  await access(resolve(APP_ROOT, asset.slice(2)));
}

console.log(
  `PWA validation passed: ${requiredAssets.size} required assets are present in APP_SHELL.`,
);
