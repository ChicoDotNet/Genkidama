# Curso de Go desde cero — Construye un monitor concurrente de uptime

Go es un lenguaje compilado de propósito general usado ampliamente en servicios de red, APIs, CLIs e infraestructura. Este curso parte desde cero y construye **UptimeLab**, un monitor local que comprueba endpoints HTTP concurrentemente, conserva historial y expone resultados, resúmenes, tendencias y diagnóstico agregado mediante API y dashboard.

El objetivo es preparación práctica para trabajo inicial: leer y escribir Go sencillo, probarlo, depurarlo, explicar sus decisiones y modificar una base existente. No promete empleo. La encuesta oficial de Go 2025 muestra APIs, CLIs e infraestructura entre los usos relevantes del ecosistema, pero también indica que Go suele aprenderse después de comenzar una carrera profesional.

## Qué vas a construir

UptimeLab crece durante 17 lecciones. Al completar el curso puedes:

- validar targets HTTP y distinguir errores de transporte de respuestas HTTP;
- medir estado y latencia;
- ejecutar checks con concurrencia acotada y cancelación mediante `context.Context`;
- persistir un historial JSON acotado y mantener consistente memoria/disco ante fallos;
- ejecutar checks periódicos con shutdown limpio;
- derivar disponibilidad, latencia media, último estado, rachas y tendencias;
- validar contratos HTTP;
- diagnosticar fallas desde evidencia reproducible;
- ejecutar `gofmt`, `go vet`, `go test -race` y build como gate profesional;
- medir peticiones/fallas/duración de forma agregada y opt-in sin guardar PII ni datos de targets;
- aplicar headers defensivos y 404 reales sin presentar la app educativa como producto de seguridad completo;
- modificar la base existente mediante una evaluación final autónoma y defender sus decisiones en entrevista.

## Requisitos

- Go **1.26.5** estable, verificado el 14-ago-2026;
- Windows 11 + PowerShell o Linux + bash;
- VS Code u otro editor, opcional.

## Instalar, build, test y run

Desde `learn/es/go/app`:

```bash
go version
gofmt -w .
go vet ./...
go test -race ./...
go build ./cmd/uptimelab
go run ./cmd/uptimelab
```

Abre `http://127.0.0.1:8080`.

Configuración típica:

```bash
UPTIMELAB_TARGETS='API=https://example.com,Go=https://go.dev' \
UPTIMELAB_INTERVAL=30s \
UPTIMELAB_HISTORY_FILE=data/demo-history.json \
go run ./cmd/uptimelab
```

La carpeta `app/data/` se ignora para no versionar historial local por accidente.

## Endpoints actuales

- `GET /health` — liveness local;
- `GET /api/checks` — ejecuta un batch y persiste antes de responder éxito;
- `GET /api/history` — evidencia histórica retenida;
- `GET /api/summary` — resumen derivado por target;
- `GET /api/trends?window=5` — ventana reciente vs anterior;
- `GET /api/diagnostics` — sólo cuando el servidor se construye con collector agregado;
- `GET /` — dashboard local.

Rutas inexistentes devuelven 404. El handler añade `nosniff`, `no-referrer` y CSP. La CSP actual todavía permite el script inline del dashboard; el curso lo documenta como límite de hardening, no como estado ideal de producción.

## Lecciones

1. [Tu primer check HTTP](lessons/01-tu-primer-check-http.md)
2. [Tipos, errores y contratos](lessons/02-tipos-errores-y-contratos.md)
3. [Concurrencia acotada con goroutines](lessons/03-concurrencia-acotada.md)
4. [API, dashboard y checkpoint 01](lessons/04-api-dashboard-y-checkpoint.md)
5. [Configuración operativa](lessons/05-configuracion-operativa.md)
6. [Historial persistente](lessons/06-historial-persistente.md)
7. [Scheduling y cancelación](lessons/07-scheduling-y-cancelacion.md)
8. [Estado consistente y checkpoint 02](lessons/08-estado-consistente-y-checkpoint.md)
9. [Resúmenes derivados del historial](lessons/09-resumenes-derivados-del-historial.md)
10. [Tendencias por ventanas](lessons/10-tendencias-por-ventanas.md)
11. [Contratos HTTP para diagnóstico](lessons/11-contratos-http-para-diagnostico.md)
12. [Diagnóstico reproducible y checkpoint 03](lessons/12-diagnostico-reproducible-y-checkpoint.md)
13. [Gate profesional de Go](lessons/13-gate-profesional-de-go.md)
14. [Debugging desde evidencia](lessons/14-debugging-desde-evidencia.md)
15. [Medir antes de optimizar](lessons/15-medir-antes-de-optimizar.md)
16. [Hardening operativo y checkpoint 04](lessons/16-hardening-y-checkpoint-04.md)
17. [Evaluación final sin receta](lessons/17-evaluacion-final.md)

## Checkpoints y evaluación

- [Checkpoint 01 — Timeout configurable](exercises/checkpoint-01.md) → [solución](solutions/checkpoint-01.md)
- [Checkpoint 02 — Historial durable sin estado fantasma](exercises/checkpoint-02.md) → [solución](solutions/checkpoint-02.md)
- [Checkpoint 03 — Señal de deterioro sin falsear la evidencia](exercises/checkpoint-03.md) → [solución](solutions/checkpoint-03.md)
- [Checkpoint 04 — Diagnóstico útil sin filtrar datos](exercises/checkpoint-04.md) → [solución](solutions/checkpoint-04.md)
- [Evaluación final](exercises/evaluacion-final.md) → [rúbrica](exercises/rubrica-final.md) → [solución de referencia](solutions/evaluacion-final.md)

## Arquitectura actual

```text
cmd/uptimelab
   ├── scheduler.Runner ─┐
   └── web.Server ───────┼→ monitor.Checker → net/http
            │            │
            ├→ history.Log → history.Store → JSON local
            ├→ insights → summary / trends derivados
            └→ RequestMetrics → agregados HTTP opt-in
```

`monitor` no conoce dashboard, archivos ni scheduling. `history` no conoce HTTP. `insights` no conoce persistencia ni query strings. `RequestMetrics` sólo observa status y duración; no recibe URLs, bodies ni targets. `cmd/uptimelab` compone las fronteras y el ciclo de vida.

## Contratos importantes

- Un HTTP 5xx de un target es un resultado válido; un error de transporte se representa por separado.
- `CheckAll` conserva el orden de targets aunque el trabajo sea concurrente.
- JSON corrupto es error y no se silencia.
- Si `Store.Save` falla, el historial visible anterior no cambia.
- Summary y trends son vistas reconstruibles, no una segunda fuente de verdad.
- La disponibilidad expresa sólo la muestra retenida localmente; no es un SLA universal.
- Diagnóstico está apagado por defecto y contiene sólo agregados.
- Los headers defensivos no sustituyen TLS, identidad, autorización ni rate limiting.

## Preguntas frecuentes

**¿Necesito saber programar?** No. Las primeras lecciones explican paquetes, structs, métodos, errores y comandos mientras la misma app crece.

**¿Por qué no usamos un framework web?** `net/http` cubre el vertical actual y mantiene visibles los fundamentos de Go.

**¿Por qué JSON y no una base de datos?** El historial es pequeño y local. La interfaz `history.Store` permite cambiarlo cuando volumen, consultas o multi-proceso lo justifiquen.

**¿Por qué no guardamos summary, trends o diagnóstico detallado?** Los dos primeros son derivados; el diagnóstico detallado ampliaría retención de datos sensibles sin necesidad educativa.

**¿Esto ya es monitoreo de producción?** No. Es una app educativa local con contratos profesionales acotados; faltan identidad, TLS gestionado, almacenamiento multi-proceso, telemetría de producción y operación distribuida.

## Glosario

- **goroutine:** función ejecutada concurrentemente por el runtime de Go.
- **channel:** mecanismo tipado de coordinación entre goroutines.
- **context:** contrato estándar para cancelación y deadlines.
- **httptest:** utilidades estándar para probar HTTP sin internet.
- **race detector:** instrumentación que detecta ciertas carreras durante ejecución de pruebas.
- **vista derivada:** información reconstruible desde la fuente durable.
- **diagnóstico opt-in:** observabilidad que sólo se habilita explícitamente.
- **CSP:** política del navegador que restringe fuentes de contenido/script.

## Cómo hablar de este proyecto en una entrevista

Explica el problema: comprobar varios endpoints sin serializar esperas de red y conservar evidencia entre reinicios. Describe límite de concurrencia, `context`, orden determinista, persistencia detrás de interfaz, rollback lógico, scheduler cancelable, análisis derivado, race detector, diagnóstico agregado y hardening HTTP. Reconoce límites: JSON single-process, retención pequeña, CSP con script inline y ausencia deliberada de claims de monitoreo/seguridad de producción.

Preguntas probables: ¿por qué limitar goroutines?, ¿qué diferencia hay entre error de transporte y HTTP 500?, ¿por qué persistir antes de actualizar memoria?, ¿cómo pruebas HTTP sin internet?, ¿qué detecta `-race`?, ¿por qué el diagnóstico no guarda URLs?, ¿qué cambiarías para millones de checks o múltiples procesos?

## Referencias oficiales

- https://go.dev/doc/
- https://go.dev/doc/devel/release
- https://pkg.go.dev/net/http
- https://pkg.go.dev/context
- https://pkg.go.dev/net/http/httptest
- https://pkg.go.dev/encoding/json
- https://pkg.go.dev/os/signal
- https://pkg.go.dev/time
- https://pkg.go.dev/sync
- https://pkg.go.dev/errors
- https://go.dev/doc/diagnostics
- https://go.dev/doc/articles/race_detector
- https://go.dev/blog/survey2025

## Siguiente paso

Usa la evaluación final para detectar áreas débiles y construye una variante propia de UptimeLab. Para continuar Genkidama Learn, el siguiente curso v1 planificado es Rust.
