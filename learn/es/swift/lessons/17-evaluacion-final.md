# Lección 17 — Evaluación final: entrega TimeQuote

## Qué vas a conseguir

Demostrar que puedes leer, modificar, probar y explicar TimeQuote sin seguir una receta paso a paso.

## Antes de empezar

Completa las lecciones 1–16 y los cuatro checkpoints. Esta evaluación no introduce conceptos nuevos: combina dominio, protocolos, persistencia, errores, concurrencia, estado de aplicación y criterio profesional.

## El problema

Un curso 0 → Junior no termina cuando puedes repetir ejemplos. Termina cuando puedes recibir una base existente, entender sus fronteras, corregir un defecto, agregar comportamiento, protegerlo con pruebas y explicar qué decisiones tomaste y cuáles decidiste no tomar.

## Encargo final

Trabaja primero con el [ejercicio final versionado](../exercises/final-timequote.md). No abras la solución de referencia hasta haber intentado las seis historias.

Debes resolver las siguientes historias sin instrucciones de implementación detalladas.

### Historia 1 — Estados de una cotización

Añade un estado explícito para una cotización: `draft`, `sent`, `accepted` o `rejected`. Impide transiciones evidentemente inválidas y protege al menos una regla con una prueba.

### Historia 2 — Corrige un defecto de persistencia

Haz que un archivo JSON truncado o incompatible produzca un error de persistencia distinguible de un libro TimeQuote legítimamente vacío. Añade una prueba de regresión.

### Historia 3 — Resumen útil por cliente

Añade una consulta que devuelva, por cliente, minutos registrados e importe acumulado. Decide si esa operación pertenece al dominio, al servicio o a una frontera de lectura y explica tu decisión.

### Historia 4 — Trabajo asíncrono realista

Introduce una operación que tenga sentido ejecutar de forma asíncrona —por ejemplo cargar/refrescar el resumen desde la frontera de aplicación— sin convertir cálculos puros o entidades en `async` por contagio.

### Historia 5 — Estado visible

Extiende `TimeQuoteViewState` para distinguir entre una primera carga y una actualización posterior que conserva datos visibles. Explica cómo una futura vista SwiftUI podría renderizar ese estado sin conocer JSON ni reglas de negocio.

### Historia 6 — Cambio de arquitectura razonado

Supón que TimeQuote debe sincronizar posteriormente con un backend. Diseña el contrato mínimo que necesitarías para sincronizar cambios locales sin reemplazar `TimeQuoteRepository` por una interfaz gigantesca. No implementes red real salvo que puedas justificarla y probarla sin introducir dependencia externa innecesaria.

## Evidencia obligatoria

Tu entrega debe incluir:

1. código Swift modificado;
2. pruebas nuevas o ajustadas para las historias relevantes;
3. `swift build`, `swift test` y `swift run TimeQuote` verdes;
4. una explicación breve de qué pertenece al núcleo portable y qué requeriría macOS/Xcode para convertirse en UI SwiftUI real;
5. README actualizado si cambió la forma de instalar, compilar, probar o ejecutar;
6. una decisión que deliberadamente **no** implementaste por considerarla sobrearquitectura o por carecer de evidencia suficiente.

## Cómo comprobar

Desde `app/`:

```bash
swift build
swift test
swift run TimeQuote
```

No sustituyas una validación específica de plataforma por la frase “debería funcionar”. El curso verifica el núcleo SwiftPM en Linux; una aplicación SwiftUI compilada requiere evidencia real con macOS/Xcode y queda fuera de lo que este runner puede certificar.

La política del repositorio tampoco exige perseguir 100% de code coverage: cuando sea medible, 44% es piso suficiente si contratos, failure modes y regresiones relevantes están protegidos; 44%–72.8% es plenamente aceptable y una cifra superior es bienvenida.

## Rúbrica — 100 puntos

| Área | Puntos | Evidencia esperada |
|---|---:|---|
| Swift idiomático y modelo de dominio | 20 | value semantics, enums, optionals y tipos usados con intención |
| Comportamiento y pruebas | 20 | reglas y regresiones protegidas con pruebas útiles |
| Persistencia y errores | 15 | almacenamiento sustituible y fallos explícitos |
| Concurrencia y aislamiento | 15 | `async/await` y `MainActor` sólo donde aportan valor |
| Estado y frontera de UI | 10 | estado observable sin reglas de negocio dentro de la vista |
| Mantenibilidad | 10 | responsabilidades claras y cambios localizados |
| Explicación profesional | 10 | decisiones, alternativas y trade-offs defendibles |

### Interpretación

- **85–100:** evidencia sólida de nivel Junior/Entry Level para este alcance.
- **70–84:** base razonable; repasa las áreas de menor puntuación antes de presentar el proyecto.
- **<70:** vuelve al checkpoint correspondiente y repite la historia que expuso la brecha.

La rúbrica mide preparación sobre este proyecto; no promete empleo.

## Cómo hablar de este proyecto en una entrevista

Prepárate para responder con ejemplos concretos del código:

1. ¿Por qué `TimeQuoteService` no conoce rutas de archivos ni JSON?
2. ¿Qué ganaste al empezar con un repositorio en memoria antes de implementar persistencia durable?
3. ¿Por qué un archivo corrupto no debe convertirse silenciosamente en un libro vacío?
4. ¿Qué partes de TimeQuote necesitan `async/await` y cuáles no?
5. ¿Por qué `TimeQuoteApplication` está aislada con `MainActor` pero el dominio no?
6. ¿Qué debería hacer SwiftUI además de renderizar estado y enviar intenciones?
7. ¿Cómo separarías persistencia local de sincronización remota si la aplicación creciera?
8. ¿Qué medirías antes de decidir migrar el almacenamiento JSON a otra tecnología?

No memorices respuestas. Señala decisiones que realmente puedas enseñar en el repositorio.

## Solución de referencia

Sólo después de intentar el ejercicio, compara tus decisiones con la [solución de referencia](../solutions/final-timequote.md). No necesitas coincidir con ella si conservas los contratos, pruebas el comportamiento y puedes explicar tus trade-offs.

## Referencias

- https://www.swift.org/documentation/
- https://docs.swift.org/swift-book/documentation/the-swift-programming-language/concurrency/
- https://developer.apple.com/documentation/swiftui
- https://developer.apple.com/documentation/swift/mainactor

## Siguiente paso

Si tu entrega satisface la rúbrica y los gates ejecutables, conserva TimeQuote como proyecto de portafolio y practica explicarlo desde el problema de negocio hacia las decisiones técnicas. Para control de versiones continúa usando el [curso transversal de Git](../../git/README.md).
