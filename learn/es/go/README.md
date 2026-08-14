# Curso de Go desde cero — Construye un monitor concurrente de uptime

Go es un lenguaje compilado de propósito general especialmente usado en servicios de red, APIs, CLIs e infraestructura. Este curso parte desde cero y construye **UptimeLab**, un monitor local que comprueba endpoints HTTP concurrentemente, conserva historial y expone resultados mediante API y dashboard.

El objetivo es preparación práctica para trabajo inicial: leer y escribir Go sencillo, probarlo, depurarlo, explicar sus decisiones y modificar una base existente. No promete empleo. La encuesta oficial de Go 2025 muestra APIs, CLIs e infraestructura entre los usos relevantes del ecosistema, pero también indica que Go suele aprenderse después de comenzar una carrera profesional; tratamos el mercado con esa realidad.

## Qué vas a construir

UptimeLab crece durante 17 lecciones. Después de las primeras ocho ya puedes:

- validar targets HTTP;
- medir estado y latencia;
- ejecutar varios checks con concurrencia acotada;
- cancelar trabajo mediante `context.Context`;
- consultar resultados desde `/api/checks`;
- persistir un historial JSON acotado;
- consultar `/api/history` después de reiniciar;
- ejecutar checks periódicos con shutdown limpio;
- mantener consistente memoria/disco cuando una persistencia falla;
- ejecutar pruebas offline con `httptest` y el detector de carreras.

## Requisitos

- Go **1.26.5** (línea estable soportada verificada el 14-ago-2026; Go 1.27 seguía en prerelease);
- Windows 11 + PowerShell o Linux + bash;
- VS Code u otro editor es opcional.

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

Para usar tus propios targets:

```bash
UPTIMELAB_TARGETS='API=https://example.com,Go=https://go.dev' go run ./cmd/uptimelab
```

Para operación periódica con historial en otra ruta:

```bash
UPTIMELAB_INTERVAL=30s UPTIMELAB_HISTORY_FILE=data/demo-history.json go run ./cmd/uptimelab
```

En PowerShell:

```powershell
$env:UPTIMELAB_TARGETS='API=https://example.com,Go=https://go.dev'
$env:UPTIMELAB_INTERVAL='30s'
$env:UPTIMELAB_HISTORY_FILE='data/demo-history.json'
go run ./cmd/uptimelab
```

La carpeta `app/data/` se ignora para no versionar historial local por accidente.

## Lecciones

1. [Tu primer check HTTP](lessons/01-tu-primer-check-http.md)
2. [Tipos, errores y contratos](lessons/02-tipos-errores-y-contratos.md)
3. [Concurrencia acotada con goroutines](lessons/03-concurrencia-acotada.md)
4. [API, dashboard y checkpoint 01](lessons/04-api-dashboard-y-checkpoint.md)
5. [Configuración operativa](lessons/05-configuracion-operativa.md)
6. [Historial persistente](lessons/06-historial-persistente.md)
7. [Scheduling y cancelación](lessons/07-scheduling-y-cancelacion.md)
8. [Estado consistente y checkpoint 02](lessons/08-estado-consistente-y-checkpoint.md)

## Checkpoints

- [Checkpoint 01 — Timeout configurable](exercises/checkpoint-01.md) → [solución](solutions/checkpoint-01.md)
- [Checkpoint 02 — Historial durable sin estado fantasma](exercises/checkpoint-02.md) → [solución](solutions/checkpoint-02.md)

## Arquitectura actual

```text
cmd/uptimelab
   ├── scheduler.Runner ─┐
   └── web.Server ───────┼→ monitor.Checker → net/http
            │            │
            └→ history.Log → history.Store → JSON local
```

`monitor` no conoce dashboard, variables de entorno, archivos ni scheduling. `history` no conoce HTTP. `scheduler` sólo conoce una operación cancelable. `cmd/uptimelab` compone las fronteras y el ciclo de vida del proceso.

## Contratos importantes

- Un HTTP 5xx de un target es un resultado válido de monitoreo; un error de transporte se representa por separado.
- El orden de resultados de `CheckAll` coincide con el orden de targets aunque el trabajo sea concurrente.
- Un historial inexistente significa primera ejecución; JSON corrupto es error y no se silencia.
- Un batch sólo se considera exitoso cuando sus resultados pudieron persistirse. Si `Store.Save` falla, el historial visible anterior no cambia.
- El scheduler es cancelable y no oculta goroutines dentro del dominio.

## Preguntas frecuentes

**¿Necesito saber programar?** No. Las primeras lecciones explican paquetes, structs, métodos, errores y comandos mientras la misma app crece.

**¿Por qué no usamos un framework web?** `net/http` cubre el vertical actual y mantiene visibles los fundamentos de Go. Añadiremos dependencias sólo si resuelven un problema real.

**¿Por qué JSON y no una base de datos?** El historial actual es pequeño y local. Una interfaz `history.Store` permite cambiar la implementación cuando volumen, consultas o multi-proceso lo justifiquen.

**¿La concurrencia significa lanzar goroutines sin límite?** No. UptimeLab usa un límite explícito y conserva el orden de entrada de los resultados.

**¿Esto ya es un producto de monitoreo de producción?** No. Es una aplicación educativa local. Aún faltan consultas/diagnóstico más ricos, observabilidad, hardening y una evaluación Junior final.

## Glosario

- **goroutine:** función ejecutada concurrentemente por el runtime de Go.
- **channel:** mecanismo tipado para coordinación/comunicación entre goroutines.
- **context:** contrato estándar para cancelación y deadlines.
- **interface:** conjunto de métodos satisfecho implícitamente por tipos compatibles.
- **httptest:** utilidades estándar para probar HTTP sin depender de servicios externos.
- **snapshot candidato:** siguiente estado que se persiste antes de sustituir el estado visible actual.
- **ticker:** fuente periódica de eventos del paquete `time`.

## Cómo hablar de este proyecto en una entrevista

Explica primero el problema: comprobar varios endpoints sin serializar esperas de red y conservar evidencia entre reinicios. Después describe límite de concurrencia, `context`, preservación del orden, inyección de cliente/clock, persistencia detrás de interfaz, rollback lógico y scheduler cancelable. Reconoce límites: el JSON es single-process y no está diseñado para millones de observaciones.

Preguntas probables: ¿por qué limitar goroutines?, ¿por qué `context.Context`?, ¿cómo pruebas HTTP sin internet?, ¿qué diferencia hay entre un error de transporte y un HTTP 500?, ¿por qué persistir antes de actualizar memoria?, ¿qué cambiarías para múltiples procesos o millones de checks?

## Referencias oficiales

- https://go.dev/doc/
- https://go.dev/doc/devel/release
- https://pkg.go.dev/net/http
- https://pkg.go.dev/context
- https://pkg.go.dev/net/http/httptest
- https://pkg.go.dev/encoding/json
- https://pkg.go.dev/os/signal
- https://pkg.go.dev/time
- https://go.dev/blog/survey2025

## Siguiente paso

Después del checkpoint 02, el curso profundizará consultas sobre historial, diagnóstico y operación profesional antes del bloque de hardening y la evaluación Junior final.
