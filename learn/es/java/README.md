# Curso de Java desde cero — Construye una API de mesa de ayuda

Aprende Java construyendo **HelpDesk API**, una aplicación local para registrar, consultar, priorizar, persistir y operar tickets de soporte. El curso parte desde cero y termina orientado a evidencia Junior/Entry Level: código que puedes ejecutar, probar, modificar y explicar.

Java se usa ampliamente en backend, sistemas empresariales, integración y servicios internos. Terminar este curso no garantiza empleo; el objetivo es desarrollar fundamentos profesionales transferibles y una aplicación defendible en entrevista.

## Qué vas a construir

HelpDesk API ya permite crear, consultar, filtrar, priorizar y avanzar tickets; persistirlos en JSON detrás de `TicketStore`; conservar consistencia local bajo concurrencia; derivar resúmenes; habilitar diagnóstico agregado opt-in sin PII; medir duración con reloj monotónico; limitar bodies JSON a 64 KiB; exigir `application/json` en mutaciones que deserializan contenido; y emitir headers HTTP defensivos. Todo está protegido con JUnit y un gate reproducible de Maven.

## Toolchain

- **Java 25 LTS**.
- **Apache Maven 3.9.x**.
- **JUnit 6.1.2**.
- Jackson 2.21 LTS en JSON HTTP y persistencia.
- Windows 11 o Linux actual; CI usa Ubuntu hospedado por GitHub.

## Instalar

Instala JDK 25 y Maven 3.9.x:

```bash
java --version
mvn --version
```

## Build y test

Desde `learn/es/java/app/`:

```bash
mvn verify
```

## Run

```bash
mvn exec:java -Dexec.mainClass=io.genkidama.learn.java.helpdesk.HelpDeskApplication
```

Por defecto escucha en `http://localhost:8080` y persiste en `data/tickets.json`. Variables disponibles:

- `HELPDESK_PORT` — puerto HTTP;
- `HELPDESK_DATA_FILE` — archivo de persistencia;
- `HELPDESK_DIAGNOSTICS=1` — habilita `/api/diagnostics` con agregados sin PII.

Prueba rápida:

```bash
curl http://localhost:8080/health
curl -X POST http://localhost:8080/api/tickets \
  -H 'Content-Type: application/json' \
  -d '{"title":"VPN caída","description":"Sin acceso remoto","priority":"HIGH"}'
curl 'http://localhost:8080/api/tickets?status=open&priority=high'
curl http://localhost:8080/api/tickets/summary
```

## Ruta del curso

1. [Tu primer ticket: records, enums y JUnit](lessons/01-tu-primer-ticket.md)
2. [Reglas de dominio, colecciones y ciclo de vida](lessons/02-reglas-dominio-y-ciclo.md)
3. [Una API HTTP real con JSON](lessons/03-api-http-y-json.md)
4. [Errores explícitos y Checkpoint 01](lessons/04-errores-y-checkpoint.md)
5. [Consultas tipadas sin esconder el dominio](lessons/05-consultas-tipadas.md)
6. [Modificar prioridad sin romper el ciclo de vida](lessons/06-modificar-prioridad.md)
7. [Persistencia JSON detrás de una frontera](lessons/07-persistencia-json.md)
8. [Persistir antes de publicar + Checkpoint 02](lessons/08-persistencia-segura-y-checkpoint.md)
9. [Resumen operativo con streams](lessons/09-resumen-operativo-y-streams.md)
10. [Concurrencia explícita y executor](lessons/10-concurrencia-y-executor.md)
11. [Diagnóstico opt-in sin PII](lessons/11-diagnostico-opt-in-sin-pii.md)
12. [Operación confiable + Checkpoint 03](lessons/12-operacion-confiable-y-checkpoint.md)
13. [Gate profesional y contratos HTTP](lessons/13-gate-profesional-y-contratos-http.md)
14. [Debugging desde evidencia](lessons/14-debugging-desde-evidencia.md)
15. [Medir antes de optimizar](lessons/15-medir-antes-de-optimizar.md)
16. [Hardening + Checkpoint 04](lessons/16-hardening-y-checkpoint-04.md)

Estado actual: **16/17 lecciones construidas**. El curso permanece `in_progress` hasta cumplir la evaluación final y el Course DoD completo.

## Checkpoints

- [Checkpoint 01 — Prioridad crítica](exercises/checkpoint-01.md) · [solución](solutions/checkpoint-01.md)
- [Checkpoint 02 — Escalamiento persistente](exercises/checkpoint-02.md) · [solución](solutions/checkpoint-02.md)
- [Checkpoint 03 — Diagnóstico que falla de forma útil](exercises/checkpoint-03.md) · [solución](solutions/checkpoint-03.md)
- [Checkpoint 04 — Rechazo temprano sin mutación](exercises/checkpoint-04.md) · [solución](solutions/checkpoint-04.md)

## Arquitectura actual

```text
HTTP/JSON + límites + headers + bounded ExecutorService
                         ↓
                   TicketService
                   ↙    ↓    ↘
              queries summary reglas
                         ↓
                    TicketStore
                   ↙           ↘
                Memory        JSON file

HTTP diagnostics opt-in → RequestMetrics (conteos + duración agregada)
```

El dominio no conoce sockets, headers, reloj, threads HTTP, variables de entorno ni Jackson. `HelpDeskHttpServer` posee transporte, workers, límites y métricas; `TicketService` conserva reglas y sólo publica una mutación después de persistir el snapshot candidato.

## Límites y hardening

Las mutaciones que leen JSON exigen `Content-Type: application/json` y aceptan como máximo 64 KiB. Todas las respuestas agregan `nosniff`, `no-referrer` y una CSP restrictiva. Estos controles no sustituyen TLS, autenticación, autorización, rate limiting ni gestión profesional de secretos en un despliegue real.

## Diagnóstico, rendimiento y privacidad

`RequestMetrics` conserva conteos agregados de respuestas/5xx y duración total monotónica. No recibe body, URL, título, descripción ni ID del ticket. La señal sirve para formular hipótesis; no se presenta como benchmark ni tracing distribuido.

## ¿Por qué no Spring todavía?

Spring es un puente laboral importante en Java empresarial, pero HelpDesk primero hace visibles records, enums, colecciones, excepciones, archivos, HTTP, concurrencia, pruebas, límites y contratos. Un framework sólo se incorpora cuando resuelve una necesidad que el alumno ya pueda reconocer.

## Qué sabrás hacer al terminar

La meta completa es leer y modificar Java idiomático, modelar reglas con tipos, usar colecciones/concurrencia/excepciones, trabajar con HTTP/JSON y persistencia, probar con JUnit, usar Maven, depurar fallos, medir antes de optimizar, consultar documentación oficial y explicar arquitectura y trade-offs.

## Cómo hablar de este proyecto en una entrevista

Explica decisiones y límites: dominio separado del transporte; tipos en lugar de strings ambiguos; persistencia detrás de interfaz; persistir antes de publicar; por qué `synchronized` protege una instancia pero no varios procesos; límites de entrada en la frontera; diagnóstico agregado sin PII; y por qué una métrica local no sustituye un benchmark de producción.

Preguntas probables:

- ¿Por qué `Ticket` y `TicketSummary` son records?
- ¿Qué problema resuelve `TicketStore`?
- ¿Cómo evitas estado fantasma si falla el disco?
- ¿Qué garantiza `synchronized` aquí?
- ¿Por qué el servidor tiene un executor acotado?
- ¿Por qué usas `System.nanoTime` para duración?
- ¿Qué diferencia hay entre `400`, `409`, `413`, `415` y `503` en esta API?
- ¿Qué seguridad aportan los headers y qué queda fuera de alcance?
- ¿Cuándo introducirías Spring Boot o una base de datos?

## FAQ

### ¿Puedo empezar sin saber programar?
Sí. La ruta introduce las piezas del lenguaje a medida que HelpDesk las necesita, pero exige escribir y ejecutar código en cada bloque.

### ¿Necesito nube o base de datos?
No. HelpDesk funciona localmente y evita servicios comerciales obligatorios.

### ¿El JSON local sirve para producción multiusuario?
No necesariamente. Es una persistencia local de un proceso; concurrencia multi-proceso y operación distribuida requieren otra estrategia.

### ¿El diagnóstico guarda los tickets?
No. Las métricas agregadas no reciben contenido de request ni identificadores.

### ¿Esto me convierte automáticamente en desarrollador Java contratado?
No. Produce práctica y evidencia inicial; contratación depende además de experiencia, mercado, entrevistas y necesidades de cada empresa.

## Glosario

- **JDK:** herramientas/runtime para compilar y ejecutar Java.
- **Maven:** build y dependencias.
- **record:** tipo conciso para datos inmutables.
- **enum:** conjunto cerrado de valores con nombre.
- **snapshot:** estado completo en un momento determinado.
- **ExecutorService:** contrato para ejecutar trabajo concurrente.
- **synchronized:** exclusión mutua mediante el monitor intrínseco de Java.
- **media type:** formato declarado de un mensaje HTTP, por ejemplo `application/json`.
- **reloj monotónico:** reloj útil para intervalos que no retrocede por ajustes de hora civil.
- **hardening:** reducción deliberada de superficie y ambigüedad operativa.
- **PII:** información personalmente identificable.

## Referencias oficiales

- [OpenJDK 25](https://openjdk.org/projects/jdk/25/)
- [Java SE 25 API](https://docs.oracle.com/en/java/javase/25/docs/api/)
- [Apache Maven](https://maven.apache.org/)
- [JUnit 6.1.2](https://docs.junit.org/6.1.2/)
- [Jackson](https://github.com/FasterXML/jackson)

## Siguiente paso

Completa las dieciséis lecciones y Checkpoint 04. La Lección 17 cerrará el curso con una evaluación Junior autónoma sin receta, rúbrica y solución de referencia.
