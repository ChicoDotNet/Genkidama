# Curso de Java desde cero — Construye una API de mesa de ayuda

Aprende Java construyendo **HelpDesk API**, una aplicación local para registrar, consultar, priorizar, persistir y operar tickets de soporte. El curso parte desde cero y termina orientado a evidencia Junior/Entry Level: código que puedes ejecutar, probar, modificar y explicar.

Java se usa ampliamente en backend, sistemas empresariales, integración y servicios internos. Terminar este curso no garantiza empleo; el objetivo es desarrollar fundamentos profesionales transferibles y una aplicación defendible en entrevista.

## Qué vas a construir

HelpDesk API permite crear, consultar, filtrar, priorizar y avanzar tickets; persistirlos en JSON detrás de `TicketStore`; conservar consistencia local bajo concurrencia; derivar resúmenes; habilitar diagnóstico agregado opt-in sin PII; medir duración con reloj monotónico; limitar bodies JSON a 64 KiB; exigir `application/json` en mutaciones que deserializan contenido; y emitir headers HTTP defensivos. Todo está protegido con JUnit y un gate reproducible de Maven.

## Toolchain

- **Java 25 LTS**.
- **Apache Maven 3.9.x**.
- **JUnit 6.1.2**.
- Jackson 2.21 LTS en JSON HTTP y persistencia.
- Windows 11 o Linux actual; CI usa Ubuntu hospedado por GitHub.

## Instalar, Build, Test y Run

Instala JDK 25 y Maven 3.9.x y comprueba `java --version` y `mvn --version`. Desde `app/` ejecuta:

```bash
mvn verify
mvn exec:java -Dexec.mainClass=io.genkidama.learn.java.helpdesk.HelpDeskApplication
```

Por defecto escucha en `http://localhost:8080` y persiste en `data/tickets.json`. `HELPDESK_PORT` cambia el puerto, `HELPDESK_DATA_FILE` cambia el archivo y `HELPDESK_DIAGNOSTICS=1` habilita `/api/diagnostics` con agregados sin PII.

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
17. [Evaluación final Junior sin receta](lessons/17-evaluacion-final-junior.md)

Estado: **17/17 lecciones completas**.

## Checkpoints y evaluación

- [Checkpoint 01](exercises/checkpoint-01.md) · [solución](solutions/checkpoint-01.md)
- [Checkpoint 02](exercises/checkpoint-02.md) · [solución](solutions/checkpoint-02.md)
- [Checkpoint 03](exercises/checkpoint-03.md) · [solución](solutions/checkpoint-03.md)
- [Checkpoint 04](exercises/checkpoint-04.md) · [solución](solutions/checkpoint-04.md)
- [Evaluación final](exercises/evaluacion-final.md) · [rúbrica](exercises/rubrica-final.md) · [solución de referencia](solutions/evaluacion-final.md)

## Arquitectura y límites

`HTTP/JSON → HelpDeskHttpServer → TicketService → TicketStore → Memory/JSON file`. El dominio no conoce sockets, headers, reloj, workers, variables de entorno ni Jackson. El servicio persiste el snapshot candidato antes de publicarlo. `synchronized` protege una instancia, no coordina varias JVM.

Las mutaciones JSON exigen `application/json` y aceptan hasta 64 KiB. Las respuestas agregan `nosniff`, `no-referrer` y CSP restrictiva. Esto no sustituye TLS, autenticación, autorización, rate limiting ni gestión profesional de secretos.

`RequestMetrics` conserva conteos agregados y duración monotónica, sin body, URL, título, descripción ni ID. Sirve para formular hipótesis; no es tracing distribuido ni benchmark.

## ¿Por qué no Spring todavía?

Spring es un puente laboral importante, pero el curso hace visibles primero records, enums, colecciones, excepciones, persistencia, HTTP, concurrencia, pruebas, límites y contratos. Al terminar puedes estudiar Spring reconociendo qué infraestructura abstrae.

## Qué sabrás hacer al terminar

Leer y modificar Java idiomático; modelar reglas con tipos; usar colecciones, streams, concurrencia y excepciones; trabajar con HTTP/JSON y persistencia; probar con JUnit; usar Maven; depurar fallos; medir antes de optimizar; consultar documentación oficial; y explicar arquitectura y trade-offs.

## Cómo hablar de este proyecto en una entrevista

Explica la evolución: dominio separado del transporte; tipos en lugar de strings ambiguos; persistencia detrás de interfaz; persistir antes de publicar; límites de `synchronized`; executor acotado; diagnóstico sin PII; límites de entrada; y por qué una métrica local no sustituye observabilidad de producción. La solución de la evaluación incluye preguntas probables para practicar.

## FAQ

### ¿Puedo empezar sin saber programar?
Sí. La ruta introduce las piezas cuando HelpDesk las necesita y exige escribir/ejecutar código.

### ¿Necesito nube o base de datos?
No. Funciona localmente y evita servicios comerciales obligatorios.

### ¿El JSON sirve para producción multiusuario?
No necesariamente. Es persistencia local de un proceso; operación distribuida requiere otra estrategia.

### ¿Esto garantiza empleo?
No. Produce práctica y evidencia inicial; contratación depende además de experiencia, mercado, entrevistas y necesidades de cada empresa.

## Glosario

- **JDK:** herramientas/runtime para compilar y ejecutar Java.
- **Maven:** build y dependencias.
- **record:** tipo conciso para datos inmutables.
- **enum:** conjunto cerrado de valores con nombre.
- **snapshot:** estado completo en un momento determinado.
- **ExecutorService:** contrato para ejecutar trabajo concurrente.
- **synchronized:** exclusión mutua mediante monitor dentro de una JVM.
- **media type:** formato declarado de un mensaje HTTP.
- **reloj monotónico:** reloj apropiado para medir intervalos.
- **hardening:** reducción deliberada de superficie y ambigüedad.
- **PII:** información personalmente identificable.

## Referencias oficiales

- [OpenJDK 25](https://openjdk.org/projects/jdk/25/)
- [Java SE 25 API](https://docs.oracle.com/en/java/javase/25/docs/api/)
- [Apache Maven](https://maven.apache.org/)
- [JUnit 6.1.2](https://docs.junit.org/6.1.2/)
- [Jackson](https://github.com/FasterXML/jackson)

## Siguiente paso

Resuelve la evaluación final sin abrir la solución, revisa la rúbrica y practica la defensa de entrevista. Después construye otra API pequeña desde cero o da el puente hacia Spring Boot identificando qué abstracciones aporta respecto de HelpDesk.
