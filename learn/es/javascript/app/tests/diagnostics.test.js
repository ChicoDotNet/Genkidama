import assert from "node:assert/strict";
import test from "node:test";
import { createDiagnostics } from "../src/diagnostics.js";

test("no mide ni escribe cuando diagnóstico está deshabilitado", () => {
  let clockReads = 0;
  const entries = [];
  const diagnostics = createDiagnostics({
    enabled: false,
    now: () => { clockReads += 1; return 10; },
    write: (entry) => entries.push(entry),
  });

  assert.equal(diagnostics.measure("render", () => 42), 42);
  assert.equal(clockReads, 0);
  assert.deepEqual(entries, []);
});

test("mide operación síncrona con reloj inyectado", () => {
  const times = [10, 12.345];
  const entries = [];
  const diagnostics = createDiagnostics({
    enabled: true,
    now: () => times.shift(),
    write: (entry) => entries.push(entry),
  });

  assert.equal(diagnostics.measure("ui.render", () => "ok"), "ok");
  assert.deepEqual(entries, [{ event: "timing", label: "ui.render", durationMs: 2.35 }]);
});

test("mide operación asíncrona y conserva su resultado", async () => {
  const times = [100, 108.2];
  const entries = [];
  const diagnostics = createDiagnostics({
    enabled: true,
    now: () => times.shift(),
    write: (entry) => entries.push(entry),
  });

  const result = await diagnostics.measureAsync("persistence.save", async () => "saved");
  assert.equal(result, "saved");
  assert.deepEqual(entries, [{ event: "timing", label: "persistence.save", durationMs: 8.2 }]);
});
