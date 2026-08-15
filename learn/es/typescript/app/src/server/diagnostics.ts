/** Resumen agregado de tráfico HTTP sin rutas, cuerpos ni datos personales. */
export interface DiagnosticsSnapshot {
  readonly totalRequests: number;
  readonly failedRequests: number;
  readonly totalDurationMs: number;
  readonly maxDurationMs: number;
}

/**
 * Acumula métricas operativas mínimas sin registrar URLs, payloads ni identificadores.
 * `record` debe recibir una duración no negativa en milisegundos.
 */
export class RequestMetrics {
  private totalRequests = 0;
  private failedRequests = 0;
  private totalDurationMs = 0;
  private maxDurationMs = 0;

  /** Registra el resultado agregado de una petición terminada. */
  public record(statusCode: number, durationMs: number): void {
    if (!Number.isFinite(durationMs) || durationMs < 0) throw new Error("La duración debe ser un número no negativo.");
    this.totalRequests += 1;
    if (statusCode >= 400) this.failedRequests += 1;
    this.totalDurationMs += durationMs;
    this.maxDurationMs = Math.max(this.maxDurationMs, durationMs);
  }

  /** Devuelve una copia inmutable de las métricas acumuladas. */
  public snapshot(): DiagnosticsSnapshot {
    return Object.freeze({
      totalRequests: this.totalRequests,
      failedRequests: this.failedRequests,
      totalDurationMs: this.totalDurationMs,
      maxDurationMs: this.maxDurationMs,
    });
  }
}
