# Evaluación final — Evoluciona HelpDesk sin receta

Trabaja sobre HelpDesk API. No abras la solución hasta completar un intento razonable.

## Historia A — Responsable opcional

Permite asignar opcionalmente un ticket a una persona mediante un identificador corto de texto.

Requisitos:

- un ticket puede permanecer sin responsable;
- cuando exista, el identificador se normaliza y valida en una regla independiente de HTTP y Jackson;
- la asignación sobrevive persistencia JSON y reinicio;
- datos anteriores sin responsable siguen cargando;
- la API permite asignar y retirar responsable;
- agrega pruebas de caso válido, inválido y compatibilidad legacy.

No se prescribe el nombre de la propiedad, método, endpoint o clase exacta.

## Historia B — Consulta de trabajo pendiente

Añade una consulta que obtenga tickets pendientes de una persona, ordenados de forma determinista por prioridad y después por ID.

Debe cumplir:

- sólo devuelve tickets asignados a esa persona;
- `RESOLVED` no aparece como pendiente;
- no muta tickets;
- no depende de orden accidental de `HashMap` ni de archivos;
- el parámetro externo se valida antes de consultar.

Incluye una prueba que demuestre el orden esperado.

## Historia C — Bugfix de integridad al restaurar

Fortalece la restauración para impedir que un archivo persistido contenga dos tickets con el mismo ID o un ticket cuyo estado sea imposible según las invariantes actuales.

La corrección debe:

- fallar de forma explícita antes de publicar el snapshot;
- conservar la política existente de no inventar datos silenciosamente;
- no dejar estado parcial visible;
- incluir una regresión de persistencia.

Documenta por qué prefieres rechazar el archivo a reconciliarlo automáticamente.

## Historia D — Conserva los contratos profesionales

Demuestra que siguen funcionando:

- `mvn verify`;
- creación, consulta, prioridad y ciclo de vida;
- persistencia JSON y continuidad de IDs;
- persistir antes de publicar;
- concurrencia dentro de una instancia;
- `413` y `415` sin mutación;
- headers defensivos;
- diagnóstico opt-in sin PII;
- smoke HTTP real.

No debilites una validación o prueba para conseguir verde.

## Historia E — Documentación y diseño

Consulta al menos dos fuentes oficiales de Java/JDK, Maven, JUnit o Jackson relacionadas con decisiones reales de tu cambio. Entrega por fuente: enlace, qué verificaste y qué decisión tomaste.

Después escribe entre 220 y 350 palabras respondiendo:

- ¿Por qué un `String` tipado sigue necesitando validación de dominio?
- ¿Dónde debe vivir el orden de prioridad de la consulta y por qué?
- ¿Qué garantiza `synchronized` aquí y qué no garantiza entre procesos?
- ¿Qué cambiarías primero para sustituir JSON por una base de datos multiusuario?
- ¿Qué medirías antes de optimizar una mesa con miles de tickets?
- ¿Qué información evitarías registrar al diagnosticar soporte real?

## Entrega

Entrega código, pruebas, comandos y resultados, comprobación manual relevante, nota de documentación oficial, respuesta de diseño y un error real encontrado con la evidencia usada para diagnosticarlo.

## Comprobación mínima

Desde `app/`:

```bash
mvn verify
mvn exec:java -Dexec.mainClass=io.genkidama.learn.java.helpdesk.HelpDeskApplication
```

Comprueba manualmente creación, asignación, consulta pendiente, reinicio con persistencia y rechazo de una entrada inválida.

Evalúate con [`rubrica-final.md`](rubrica-final.md).
