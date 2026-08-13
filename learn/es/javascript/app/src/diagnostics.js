/** Round a duration without pretending sub-hundredth-millisecond precision matters here. */
function roundDuration(value) {
  return Math.round(value * 100) / 100;
}

/**
 * Create opt-in timing diagnostics with injectable clock/output for deterministic tests.
 * @param {{enabled?:boolean, now?:()=>number, write?:(entry:object)=>void}} [options]
 */
export function createDiagnostics({
  enabled = false,
  now = () => performance.now(),
  write = (entry) => console.debug("[Kanban Local]", entry),
} = {}) {
  function record(label, startedAt) {
    write({
      event: "timing",
      label,
      durationMs: roundDuration(now() - startedAt),
    });
  }

  function measure(label, operation) {
    if (!enabled) return operation();
    const startedAt = now();
    try {
      return operation();
    } finally {
      record(label, startedAt);
    }
  }

  async function measureAsync(label, operation) {
    if (!enabled) return operation();
    const startedAt = now();
    try {
      return await operation();
    } finally {
      record(label, startedAt);
    }
  }

  return Object.freeze({ measure, measureAsync });
}
