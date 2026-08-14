# Curso de Go desde cero — Construye un monitor concurrente de uptime

Go es un lenguaje compilado de propósito general especialmente usado en servicios de red, APIs, CLIs e infraestructura. Este curso parte desde cero y construye **UptimeLab**, un monitor local que comprueba endpoints HTTP concurrentemente y expone resultados mediante API y dashboard.

El objetivo es preparación práctica para trabajo inicial: leer y escribir Go sencillo, probarlo, depurarlo, explicar sus decisiones y modificar una base existente. No promete empleo. La encuesta oficial de Go 2025 muestra que APIs y CLIs siguen entre los usos principales del ecosistema, pero también indica que Go suele aprenderse después de comenzar una carrera profesional; trataremos el mercado con esa realidad.

## Qué vas a construir

UptimeLab crece durante 17 lecciones. En el primer bloque ya podrás:

- validar targets HTTP;
- medir estado y latencia;
- ejecutar varios checks con concurrencia acotada;
- cancelar trabajo mediante `context.Context`;
- consultar resultados desde `/api/checks`;
- abrir un dashboard web local;
- ejecutar pruebas offline con `httptest`.

## Requisitos

- Go **1.26.5** (línea estable soportada verificada el 14-ago-2026; Go 1.27 seguía en RC);
- Windows 11 + PowerShell o Linux + bash;
- VS Code u otro editor es opcional.

## Instalar, build, test y run

Desde `learn/es/go/app`:

```bash
go version
go test ./...
go vet ./...
go build ./cmd/uptimelab
go run ./cmd/uptimelab
```

Abre `http://127.0.0.1:8080`.

Para usar tus propios targets:

```bash
UPTIMELAB_TARGETS='API=https://example.com,Go=https://go.dev' go run ./cmd/uptimelab
```

En PowerShell:

```powershell
$env:UPTIMELAB_TARGETS='API=https://example.com,Go=https://go.dev'
go run ./cmd/uptimelab
```

## Lecciones

1. [Tu primer check HTTP](lessons/01-tu-primer-check-http.md)
2. [Tipos, errores y contratos](lessons/02-tipos-errores-y-contratos.md)
3. [Concurrencia acotada con goroutines](lessons/03-concurrencia-acotada.md)
4. [API, dashboard y checkpoint 01](lessons/04-api-dashboard-y-checkpoint.md)

## Checkpoints

- [Checkpoint 01 — Timeout configurable](exercises/checkpoint-01.md) → [solución de referencia](solutions/checkpoint-01.md)

## Arquitectura actual

```text
cmd/uptimelab → web.Server → monitor.Checker → net/http
                    ↑              ↑
                 HTTP/UI       reglas + concurrencia
```

El paquete `monitor` no conoce dashboard, configuración de entorno ni proceso HTTP servidor. `web` depende de una interfaz mínima para probar sin red externa.

## Preguntas frecuentes

**¿Necesito saber programar?** No. Las primeras lecciones explican paquetes, structs, métodos, errores y comandos mientras la misma app crece.

**¿Por qué no usamos un framework web?** `net/http` ya permite construir este vertical profesional y mantiene visibles los fundamentos de Go. Añadiremos dependencias sólo si resuelven un problema real.

**¿La concurrencia significa lanzar goroutines sin límite?** No. UptimeLab usa un límite explícito y conserva el orden de entrada de los resultados.

**¿Esto es un producto de monitoreo de producción?** No todavía. Es una aplicación educativa local. Persistencia, operación continua, observabilidad y hardening se incorporan después.

## Glosario

- **goroutine:** función ejecutada concurrentemente por el runtime de Go.
- **channel:** mecanismo tipado para coordinación/comunicación entre goroutines.
- **context:** contrato estándar para cancelación, deadlines y valores de alcance de petición.
- **interface:** conjunto de métodos satisfecho implícitamente por tipos compatibles.
- **httptest:** utilidades estándar para probar HTTP sin depender de servicios externos.

## Cómo hablar de este proyecto en una entrevista

Explica primero el problema: comprobar varios endpoints sin serializar esperas de red. Después describe el límite de concurrencia, `context`, preservación del orden, inyección de cliente/clock y pruebas con `httptest`. Reconoce límites: todavía no hay persistencia histórica ni scheduler de larga duración.

Preguntas probables: ¿por qué limitar goroutines?, ¿por qué `context.Context`?, ¿cómo pruebas HTTP sin internet?, ¿qué diferencia hay entre un error de transporte y un HTTP 500?, ¿qué cambiarías para miles de targets?

## Referencias oficiales

- https://go.dev/doc/
- https://go.dev/doc/devel/release
- https://pkg.go.dev/net/http
- https://pkg.go.dev/context
- https://pkg.go.dev/net/http/httptest
- https://go.dev/blog/survey2025

## Siguiente paso

Después del checkpoint 01, el curso incorporará configuración más rica, historial/persistencia, scheduling, diagnóstico y hardening antes de la evaluación Junior final.
