# Curso de Java desde cero — Construye una API de mesa de ayuda

Aprende Java construyendo **HelpDesk API**, una aplicación local para registrar, consultar, priorizar y persistir tickets de soporte. El curso parte desde cero y termina orientado a evidencia Junior/Entry Level: código que puedes ejecutar, probar, modificar y explicar.

Java se usa ampliamente en backend, sistemas empresariales, integración y servicios internos. Eso no significa que terminar un curso garantice empleo; el objetivo es desarrollar fundamentos profesionales transferibles y una aplicación defendible en entrevista.

## Qué vas a construir

HelpDesk API crece durante todo el curso. En el estado actual ya puedes:

- crear tickets JSON por HTTP;
- listar y filtrar por `status` y `priority`;
- usar prioridades `LOW`, `NORMAL` y `HIGH`;
- cambiar prioridad sin alterar el ciclo de vida;
- avanzar `OPEN → IN_PROGRESS → RESOLVED`;
- persistir tickets en un archivo JSON local y restaurarlos después de reiniciar;
- distinguir primera ejecución, datos persistidos corruptos y fallos de escritura;
- evitar estado visible que no alcanzó a persistirse;
- recibir errores HTTP explícitos para entrada inválida, tickets ausentes, conflictos y persistencia no disponible;
- probar dominio, persistencia y servidor con JUnit.

## Toolchain

- **Java 25 LTS**.
- **Apache Maven 3.9.x**.
- **JUnit 6.1.2**.
- Jackson 2.21 LTS en JSON HTTP y persistencia.
- Windows 11 o Linux actual; CI usa Ubuntu hospedado por GitHub.

Java 26 es más reciente, pero Java 25 es la LTS vigente y es la línea elegida para material educativo con continuidad razonable.

## Instalar

Instala un JDK 25 y Maven 3.9.x. Comprueba:

```bash
java --version
mvn --version
```

En Windows puedes ejecutar los mismos comandos desde PowerShell.

## Build y test

Desde `learn/es/java/app/`:

```bash
mvn verify
```

## Run

```bash
mvn exec:java -Dexec.mainClass=io.genkidama.learn.java.helpdesk.HelpDeskApplication
```

Por defecto escucha en `http://localhost:8080` y persiste en `data/tickets.json`. Puedes usar `HELPDESK_PORT` y `HELPDESK_DATA_FILE` para cambiar ambos valores.

Prueba rápida:

```bash
curl http://localhost:8080/health
curl -X POST http://localhost:8080/api/tickets \
  -H 'Content-Type: application/json' \
  -d '{"title":"VPN caída","description":"Sin acceso remoto","priority":"HIGH"}'
curl 'http://localhost:8080/api/tickets?status=open&priority=high'
curl -X PUT http://localhost:8080/api/tickets/1/priority \
  -H 'Content-Type: application/json' \
  -d '{"priority":"NORMAL"}'
curl -X POST http://localhost:8080/api/tickets/1/advance
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

Estado actual: **8/17 lecciones construidas**. El curso permanece `in_progress` hasta cumplir el Course DoD completo.

## Checkpoints

- [Checkpoint 01 — Prioridad crítica](exercises/checkpoint-01.md) · [solución](solutions/checkpoint-01.md)
- [Checkpoint 02 — Escalamiento persistente](exercises/checkpoint-02.md) · [solución](solutions/checkpoint-02.md)

## Arquitectura actual

```text
HTTP + JSON
    ↓
TicketService ← TicketQuery
    ↓
Ticket / enums / reglas
    ↓
TicketStore
  ↙       ↘
Memory   JSON file
```

El dominio no conoce sockets, rutas ni Jackson. `HelpDeskHttpServer` traduce HTTP; `JsonFileTicketStore` traduce archivos; `TicketService` conserva las reglas y sólo publica una mutación después de que el store acepta el snapshot candidato.

## ¿Por qué JSON antes que una base de datos?

La necesidad visible es sobrevivir un reinicio y practicar una frontera de persistencia. JSON ya puede resolverla con la dependencia existente y mantiene el mecanismo observable. No es una afirmación de que JSON sea apropiado para múltiples procesos, alta concurrencia o grandes volúmenes. La interfaz `TicketStore` deja ese límite explícito para un incremento futuro.

## ¿Por qué no Spring todavía?

Spring es un puente laboral importante en Java empresarial, pero el curso primero hace visibles records, enums, colecciones, excepciones, archivos, HTTP, pruebas y contratos. Introducir un framework sólo tiene sentido cuando resuelva una necesidad que el alumno ya pueda reconocer.

## Qué sabrás hacer al terminar

La meta del curso completo es que puedas leer y modificar Java idiomático sencillo, modelar reglas con tipos, usar colecciones y excepciones, trabajar con HTTP/JSON y persistencia, probar con JUnit, usar Maven, depurar fallos, consultar Javadoc/documentación oficial y explicar la arquitectura de una aplicación existente.

## Cómo hablar de este proyecto en una entrevista

Explica decisiones y límites: dominio separado del transporte, tipos en lugar de strings ambiguos, persistencia detrás de una interfaz, diferencia entre archivo ausente y corrupto y por qué HelpDesk persiste un candidato antes de hacerlo visible en memoria.

Preguntas probables:

- ¿Por qué `Ticket` es un `record`?
- ¿Por qué `TicketQuery` usa enums?
- ¿Qué problema resuelve `TicketStore`?
- ¿Qué garantiza y qué no garantiza el reemplazo de un snapshot JSON?
- ¿Cómo evitas estado fantasma si falla el disco?
- ¿Cuándo cambiarías JSON por una base de datos?
- ¿Cuándo introducirías Spring Boot y por qué?

## FAQ

### ¿Puedo empezar sin saber programar?
Sí. La ruta introduce las piezas del lenguaje a medida que HelpDesk las necesita, pero exige escribir y ejecutar código en cada bloque.

### ¿Necesito una nube o base de datos?
No. HelpDesk funciona localmente y evita servicios comerciales obligatorios.

### ¿El JSON local sirve para producción multiusuario?
No necesariamente. Es una persistencia didáctica y útil para una aplicación local de un proceso; concurrencia multi-proceso y operación distribuida requieren otra estrategia.

### ¿Esto me convierte automáticamente en desarrollador Java contratado?
No. Produce práctica y evidencia inicial; contratación depende además de experiencia, mercado, entrevistas y necesidades de cada empresa.

## Glosario

- **JDK:** herramientas y runtime para compilar/ejecutar Java.
- **Maven:** herramienta de build y dependencias.
- **record:** tipo conciso para datos inmutables.
- **enum:** conjunto cerrado de valores con nombre.
- **dominio:** reglas del problema independientes de transporte/persistencia.
- **adapter:** capa que traduce una tecnología externa a un contrato interno.
- **snapshot:** representación completa del estado en un momento determinado.
- **persistencia:** almacenamiento que sobrevive al proceso actual.

## Referencias oficiales

- [OpenJDK 25](https://openjdk.org/projects/jdk/25/)
- [Java SE 25 API](https://docs.oracle.com/en/java/javase/25/docs/api/)
- [Apache Maven](https://maven.apache.org/)
- [JUnit 6.1.2](https://docs.junit.org/6.1.2/)
- [Jackson](https://github.com/FasterXML/jackson)

## Siguiente paso

Completa las primeras ocho lecciones y Checkpoint 02. El siguiente bloque profundizará en contratos de aplicación, diagnóstico y operación antes de decidir si un framework aporta suficiente valor.
