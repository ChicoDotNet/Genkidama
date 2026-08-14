# Lección 17 — Evaluación final Junior sin receta

## Qué vas a conseguir

Vas a demostrar que puedes leer, modificar, probar y defender HelpDesk sin seguir una secuencia de cambios prescrita.

## Antes de empezar

Completa la [Lección 16](16-hardening-y-checkpoint-04.md), ejecuta `mvn verify` y haz una copia de trabajo limpia. No abras la solución de referencia.

## El problema

En un trabajo Junior rara vez recibirás “edita esta línea y pega este método”. Recibirás una historia, una base existente, restricciones y evidencia de que algo debe seguir funcionando. La evaluación reproduce esa situación a escala segura.

## Concepto

La competencia aquí no es memorizar Java. Es mantener contratos mientras cambias comportamiento: comprender tipos, localizar fronteras, validar datos externos, preservar integridad, escribir una regresión, consultar documentación y explicar trade-offs.

## Demostración

[DEMO] Antes de empezar, recorre el proyecto sin editarlo. Identifica dominio, servicio, store, servidor HTTP y tests. Explica en voz alta dónde esperarías tocar código para una regla de ticket y dónde para una regla de protocolo.

No se demuestra la solución.

## Código real

La evaluación trabaja sobre la misma HelpDesk API construida durante el curso. No hay proyecto de juguete alternativo ni starter oculto.

Abre [`../exercises/evaluacion-final.md`](../exercises/evaluacion-final.md) y resuelve sus historias en el orden que consideres adecuado.

## Qué acaba de pasar

Ahora el diseño existente es parte del problema. Una solución correcta que rompe persistencia, concurrencia local, HTTP o privacidad no es una evolución correcta del sistema.

## Errores comunes

- Abrir la solución antes de intentar diseñar.
- Meter validación de dominio directamente en el handler HTTP.
- Depender del orden accidental de una colección.
- Cambiar JSON sin pensar en archivos anteriores.
- Corregir una prueba en lugar del bug que reveló.
- Afirmar que `synchronized` protege varias JVM.
- Registrar tickets completos para “depurar más fácil”.

## Buenas prácticas

Trabaja en ciclos pequeños: reproduce, escribe una prueba cuando corresponda, cambia lo mínimo, ejecuta `mvn verify` y conserva notas de evidencia. Si consultas documentación, registra qué duda resolviste.

## Tu turno

[PAUSA PARA EJERCICIO] Resuelve la [evaluación final](../exercises/evaluacion-final.md). Reserva tiempo para la nota de diseño y la comprobación manual; forman parte de la competencia evaluada.

## Cómo comprobar

Como mínimo:

```bash
cd app
mvn verify
mvn exec:java -Dexec.mainClass=io.genkidama.learn.java.helpdesk.HelpDeskApplication
```

Usa además la [rúbrica](../exercises/rubrica-final.md) para revisar funcionalidad, integridad, pruebas, calidad Java, tooling y defensa profesional.

## Solución enlazada

Sólo después de entregar tu intento, compara con la [solución de referencia](../solutions/evaluacion-final.md). Una implementación diferente puede ser mejor si conserva los contratos y puedes defenderla con evidencia.

## Reto adicional

Escribe un ADR de una página comparando JSON local contra PostgreSQL para una versión multiusuario. No implementes la migración: define fuerzas, riesgos, criterio de decisión y qué frontera existente facilita el cambio.

## Resumen

- Modificar una base existente es distinto de copiar un tutorial.
- Los tipos no sustituyen validación de datos externos.
- Una regresión protege una decisión concreta.
- Documentación oficial forma parte del trabajo profesional.
- Explicar límites es tan importante como explicar capacidades.

## Siguiente paso

Si completaste la evaluación con evidencia, revisa la rúbrica y practica la defensa de entrevista incluida en la solución. Después el puente natural es construir otra API pequeña desde cero o aprender Spring Boot identificando qué infraestructura abstrae respecto de HelpDesk.

## Referencias

- [Java SE 25 API](https://docs.oracle.com/en/java/javase/25/docs/api/)
- [Apache Maven](https://maven.apache.org/)
- [JUnit 6.1.2](https://docs.junit.org/6.1.2/)
- [Jackson](https://github.com/FasterXML/jackson)
