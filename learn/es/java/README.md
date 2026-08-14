# Curso de Java desde cero — Construye una API de mesa de ayuda

Aprende Java construyendo **HelpDesk API**, una aplicación local para registrar tickets de soporte, asignar prioridad y avanzar su atención. El curso parte desde cero y termina orientado a evidencia Junior/Entry Level: código que puedes ejecutar, probar, modificar y explicar.

Java se usa ampliamente en backend, sistemas empresariales, integración y servicios internos. Eso no significa que terminar un curso garantice empleo; el objetivo es desarrollar fundamentos profesionales transferibles y una aplicación defendible en entrevista.

## Qué vas a construir

HelpDesk API crece durante todo el curso. En el primer incremento ya puedes:

- crear tickets JSON por HTTP;
- listar tickets;
- usar prioridades `LOW`, `NORMAL` y `HIGH`;
- avanzar `OPEN → IN_PROGRESS → RESOLVED`;
- recibir errores HTTP explícitos para datos inválidos, tickets ausentes y transiciones imposibles;
- ejecutar reglas de dominio sin depender de HTTP;
- probar dominio y servidor con JUnit.

La primera implementación conserva tickets en memoria para mantener visible el lenguaje y las fronteras. Persistencia y operación más profunda llegarán sólo cuando resuelvan un problema real.

## Toolchain

- **Java 25 LTS**.
- **Apache Maven 3.9.x**.
- **JUnit 6.1.2**.
- Jackson 2.21 LTS en la frontera JSON.
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

Por defecto escucha en `http://localhost:8080`. Puedes usar otro puerto con `HELPDESK_PORT`.

Prueba rápida:

```bash
curl http://localhost:8080/health
curl -X POST http://localhost:8080/api/tickets \
  -H 'Content-Type: application/json' \
  -d '{"title":"VPN caída","description":"Sin acceso remoto","priority":"HIGH"}'
curl http://localhost:8080/api/tickets
curl -X POST http://localhost:8080/api/tickets/1/advance
```

## Ruta del curso

1. [Tu primer ticket: records, enums y JUnit](lessons/01-tu-primer-ticket.md)
2. [Reglas de dominio, colecciones y ciclo de vida](lessons/02-reglas-dominio-y-ciclo.md)
3. [Una API HTTP real con JSON](lessons/03-api-http-y-json.md)
4. [Errores explícitos y Checkpoint 01](lessons/04-errores-y-checkpoint.md)

Estado actual: **4/17 lecciones construidas**. El curso permanece `in_progress` hasta cumplir el Course DoD completo.

## Checkpoints

- [Checkpoint 01 — Prioridad crítica](exercises/checkpoint-01.md) · [solución de referencia](solutions/checkpoint-01.md)

## Arquitectura inicial

```text
HTTP + JSON
    ↓
TicketService
    ↓
Ticket / enums / reglas
```

El dominio no conoce sockets, JSON ni Maven. `HelpDeskHttpServer` traduce la frontera externa a llamadas del dominio. Esa separación permite probar reglas sin arrancar un servidor y cambiar la tecnología HTTP más adelante sin reescribir la lógica de tickets.

## ¿Por qué no Spring desde la primera pantalla?

Spring es un puente laboral importante en Java empresarial, pero introducirlo antes de comprender records, enums, colecciones, excepciones, clases, pruebas y contratos HTTP ocultaría demasiado del lenguaje. HelpDesk conserva una frontera que permite migrar/adaptar el servidor después. El curso enseñará el lenguaje primero y conectará con el ecosistema profesional donde aporte valor concreto.

## Qué sabrás hacer al terminar

La meta del curso completo es que puedas leer y modificar Java idiomático sencillo, modelar reglas con tipos, usar colecciones y excepciones, trabajar con HTTP/JSON y persistencia, probar con JUnit, usar Maven, depurar fallos, consultar Javadoc/documentación oficial y explicar la arquitectura de una aplicación existente.

## Cómo hablar de este proyecto en una entrevista

No digas sólo “hice una API”. Explica decisiones y límites: dominio separado del transporte, validación en fronteras, estados explícitos, pruebas deterministas y por qué la primera persistencia no debe añadirse antes de necesitarla.

Preguntas probables:

- ¿Por qué `Ticket` es un `record`?
- ¿Qué diferencia hay entre un `enum` y un `String` para un estado?
- ¿Por qué `TicketService` no conoce HTTP?
- ¿Cómo distingues 400, 404 y 409?
- ¿Qué cambiarías para persistir tickets?
- ¿Cuándo introducirías Spring Boot y por qué?

## FAQ

### ¿Puedo empezar sin saber programar?
Sí. La ruta introduce las piezas del lenguaje a medida que HelpDesk las necesita, pero exige escribir y ejecutar código en cada bloque.

### ¿Necesito una nube o base de datos?
No para comenzar. El curso funciona localmente y evita servicios comerciales obligatorios.

### ¿Esto me convierte automáticamente en desarrollador Java contratado?
No. Produce práctica y evidencia inicial; contratación depende además de experiencia, mercado, entrevistas y necesidades de cada empresa.

## Glosario

- **JDK:** herramientas y runtime para compilar/ejecutar Java.
- **Maven:** herramienta de build y dependencias.
- **record:** tipo conciso para datos inmutables.
- **enum:** conjunto cerrado de valores con nombre.
- **dominio:** reglas del problema independientes de transporte/persistencia.
- **HTTP adapter:** capa que traduce requests/responses a llamadas de aplicación.

## Referencias oficiales

- [OpenJDK 25](https://openjdk.org/projects/jdk/25/)
- [Java SE 25 API](https://docs.oracle.com/en/java/javase/25/docs/api/)
- [Apache Maven](https://maven.apache.org/)
- [JUnit 6.1.2](https://docs.junit.org/6.1.2/)
- [Jackson](https://github.com/FasterXML/jackson)

## Siguiente paso

Completa las primeras cuatro lecciones y Checkpoint 01. El siguiente incremento profundizará en edición/consultas y persistencia cuando exista una necesidad observable.
