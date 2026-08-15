# 17 — Evaluación final: entrega FieldFlow

## Qué vas a conseguir
Demostrar que puedes leer, modificar, probar y explicar FieldFlow sin seguir una receta paso a paso.

## Antes de empezar
Completa las lecciones 1–16 y los cuatro checkpoints. Esta evaluación no introduce conceptos nuevos.

## El problema
Un curso 0 → Junior no termina cuando puedes repetir los ejemplos. Termina cuando puedes recibir una base existente, entender sus fronteras, corregir un defecto, añadir comportamiento, protegerlo con pruebas y justificar tus decisiones.

## Encargo final
Trabaja sobre FieldFlow como si recibieras una historia de mantenimiento real. Debes entregar las siguientes seis historias sin instrucciones de implementación detalladas.

### Historia 1 — Nueva prioridad operativa
Añade una prioridad `CRITICAL` que aparezca antes que `HIGH`. Conserva compatibilidad con las órdenes ya persistidas y demuestra con pruebas el orden esperado.

### Historia 2 — Corrige un defecto de persistencia
Identifica o introduce de forma controlada un caso donde un archivo de almacenamiento inválido produce un resultado ambiguo. Haz que el fallo sea explícito y escribe una prueba de regresión.

### Historia 3 — Consulta útil para campo
Añade una consulta que devuelva únicamente órdenes abiertas y ordenadas por prioridad. Decide si pertenece al servicio, al repositorio o a ambos y explica por qué.

### Historia 4 — Cambio Android
Diseña y materializa el cambio equivalente en la frontera Room: entidad, mapping y consulta DAO necesarios para soportar la nueva prioridad y la consulta de órdenes abiertas. No contamines el dominio con anotaciones Android.

### Historia 5 — Estado de UI
Extiende el modelo de estado para representar una operación de guardado o sincronización en progreso sin bloquear la lectura de órdenes ya disponibles. Explica cómo lo renderizaría Compose.

### Historia 6 — Operación offline
Define qué ocurre si una orden se completa sin red y posteriormente el servidor informa que la orden ya había sido cancelada. No uses “última escritura gana” sin justificarlo: modela una decisión explícita o un conflicto visible.

## Evidencia obligatoria

Tu entrega debe incluir:

1. código Kotlin modificado;
2. pruebas nuevas o ajustadas para las historias 1–3;
3. evidencia ejecutable disponible para el slice Android cuando el entorno lo permita;
4. una nota breve que separe qué verificaste en JVM y qué requiere Android/Room/Compose;
5. un README actualizado si cambió la forma de instalar, compilar, probar o ejecutar;
6. una explicación de una decisión que deliberadamente **no** implementaste por considerarla sobrearquitectura.

## Cómo comprobar

Como mínimo, desde el módulo JVM:

```bash
gradle test
gradle run
```

Para el módulo Android, usa sus tareas de build/test cuando exista en tu copia. No sustituyas una validación Android faltante por una afirmación de que “debería funcionar”.

La política del repositorio no exige perseguir 100% de code coverage: cuando sea medible, 44% es piso suficiente si los contratos, failure modes y regresiones relevantes están protegidos; 44%–72.8% es plenamente aceptable y una cifra superior es bienvenida.

## Rúbrica — 100 puntos

| Área | Puntos | Evidencia esperada |
|---|---:|---|
| Kotlin idiomático y modelo de dominio | 20 | nullability, tipos y colecciones usados con intención |
| Comportamiento y pruebas | 20 | pruebas útiles de reglas y regresiones, no tests cosméticos |
| Persistencia y errores | 15 | almacenamiento sustituible y fallos explícitos |
| Android / Room / Compose | 15 | fronteras correctas y evidencia proporcional al entorno |
| Offline first | 10 | pendientes, reintentos y conflictos razonados |
| Mantenibilidad | 10 | responsabilidades claras, cambios localizados |
| Explicación profesional | 10 | puedes defender decisiones y trade-offs |

### Interpretación

- **85–100:** evidencia sólida de nivel Junior/Entry Level para este alcance.
- **70–84:** base razonable; repasa las áreas de menor puntuación antes de presentar el proyecto.
- **<70:** vuelve al checkpoint correspondiente y repite la historia que expuso la brecha.

La rúbrica mide preparación sobre este proyecto; no promete empleo.

## Cómo hablar de este proyecto en una entrevista

Prepárate para responder con ejemplos concretos:

1. ¿Por qué mantuviste `WorkOrder` independiente de Room?
2. ¿Qué ventaja tuvo comenzar con repositorios in-memory/archivo antes de Android?
3. ¿Qué diferencia hay entre un error de almacenamiento y una lista vacía legítima?
4. ¿Cómo probarías ViewModel y Compose sin convertir todas las pruebas en instrumentadas?
5. ¿Qué significa offline first en FieldFlow además de “guardar localmente”?
6. ¿Cómo decidirías un conflicto entre cambios locales y remotos?
7. ¿Qué cambiarías si FieldFlow tuviera 100,000 órdenes?

No memorices respuestas. Usa decisiones que realmente puedas señalar en el código.

## Referencias

- https://kotlinlang.org/docs/home.html
- https://developer.android.com/topic/architecture
- https://developer.android.com/training/data-storage/room
- https://developer.android.com/topic/architecture/data-layer/offline-first
- https://developer.android.com/develop/ui/compose/testing

## Siguiente paso

Si tu entrega satisface la rúbrica y los gates ejecutables, conserva FieldFlow como proyecto de portafolio y practica explicarlo desde el problema de negocio hacia las decisiones técnicas. Para control de versiones continúa usando el [curso transversal de Git](../../git/README.md).
