# Solución de referencia — Evaluación final Java

Esta es una dirección defendible, no una receta única. Compara decisiones después de completar tu intento.

## Responsable opcional

Una solución pequeña extiende el modelo persistido con un responsable nullable/ausente y concentra normalización en una función de dominio: recortar espacios, rechazar vacío cuando se intenta asignar y aplicar un límite razonable. `TicketService` expone una mutación que construye un snapshot candidato, llama a `TicketStore.save()` y sólo después sustituye el estado visible, igual que las demás mutaciones.

Para compatibilidad, Jackson debe poder leer documentos anteriores sin el nuevo campo. No conviertas la ausencia legacy en un error si semánticamente significa “sin asignar”.

## Consulta pendiente

La consulta pertenece al servicio/dominio de aplicación, no al handler HTTP. Filtra por responsable normalizado, excluye `RESOLVED` y aplica un `Comparator` explícito. Una opción razonable es definir el orden de prioridad como `CRITICAL`, `HIGH`, `MEDIUM`, `LOW` y desempatar por ID. La prueba debe declarar ese contrato; no dependas del orden interno de una colección.

## Integridad al restaurar

Valida el snapshot completo antes de construir el estado operativo. Mantén un conjunto de IDs vistos y rechaza el documento al encontrar duplicados. Valida también valores/invariantes persistidos con las mismas reglas relevantes del dominio. Rechazar es preferible a reconciliar automáticamente porque el programa no posee información suficiente para saber cuál registro representa la intención correcta.

## Pruebas mínimas esperadas

- asignar, retirar y persistir responsable;
- leer JSON legacy;
- rechazar responsable inválido;
- ordenar pendientes de forma determinista;
- rechazar snapshot con ID duplicado sin estado parcial;
- conservar todas las regresiones existentes, incluido fallo de `save()`.

## Documentación

Fuentes apropiadas incluyen Java SE 25 para `Comparator`, colecciones/concurrencia; Maven para el lifecycle de `verify`; JUnit 6 para assertions/tests; y Jackson para compatibilidad de propiedades. La evaluación pide explicar qué decisión concreta se apoyó en cada fuente, no sólo listar enlaces.

## Diseño defendible

`String` expresa representación, no reglas del negocio. `synchronized` serializa acceso al monitor dentro del proceso; no coordina otra JVM ni convierte un archivo JSON en almacenamiento transaccional multiusuario. Si HelpDesk necesitara múltiples instancias, la primera frontera a sustituir sería `TicketStore`, conservando al servicio independiente del motor concreto y revisando la estrategia de concurrencia/IDs.

Antes de optimizar, mediría volumen, distribución de consultas, latencia y costo de persistencia con datos representativos. Evitaría registrar títulos, descripciones, cuerpos, identificadores personales o payloads completos.

## Cómo hablar de este proyecto en una entrevista

Cuenta la evolución, no sólo el resultado: empezaste con records/enums y una regla pequeña; añadiste HTTP y persistencia detrás de fronteras; protegiste la publicación de estado frente a fallos de disco; hiciste explícita la concurrencia de una instancia; añadiste diagnóstico agregado sin PII; y endureciste la frontera HTTP con límites verificables.

Preguntas probables:

- ¿Por qué `TicketStore` es una interfaz?
- ¿Por qué persistes antes de cambiar memoria?
- ¿Qué problema evita un comparator explícito?
- ¿Qué ocurre si dos JVM escriben el mismo archivo?
- ¿Cómo migrarías a PostgreSQL sin mover reglas al controlador?
- ¿Cuándo introducirías Spring Boot?
- ¿Qué información no registrarías en producción y por qué?

Una respuesta fuerte reconoce límites. HelpDesk demuestra fundamentos Java y backend; no pretende ser por sí solo una arquitectura distribuida de producción.
