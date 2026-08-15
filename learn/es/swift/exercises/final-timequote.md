# Ejercicio final — Mantén y evoluciona TimeQuote

No abras la solución de referencia hasta completar un primer intento verificable.

## Contexto

Recibes TimeQuote como una base Swift ya existente. Tu trabajo no es reescribirla, sino mantenerla y extenderla conservando sus fronteras: dominio, servicio, repositorio, persistencia y estado de aplicación.

## Historias

1. Agrega estados de cotización (`draft`, `sent`, `accepted`, `rejected`) y protege una transición inválida con una prueba.
2. Distingue un archivo JSON corrupto de un estado vacío legítimo y añade una prueba de regresión.
3. Agrega un resumen por cliente con minutos e importe acumulado y justifica en qué capa vive.
4. Añade una operación asíncrona razonable sin convertir innecesariamente todo el dominio en `async`.
5. Distingue en `TimeQuoteViewState` entre primera carga y refresco conservando datos visibles.
6. Diseña un contrato mínimo para futura sincronización remota sin acoplar el dominio ni sustituir el repositorio por una interfaz gigantesca.

## Restricciones

- No muevas reglas de negocio a una futura `View`.
- No hagas que `TimeQuoteService` conozca JSON, rutas o SwiftUI.
- No ocultes corrupción de datos devolviendo silenciosamente un estado vacío.
- No introduzcas red real o un framework adicional si no existe una razón demostrable.
- No persigas 100% de code coverage; protege comportamiento y failure modes relevantes.

## Evidencia

Entrega:

- código modificado;
- pruebas nuevas o modificadas;
- salida verde de `swift build`, `swift test` y `swift run TimeQuote`;
- una explicación de la frontera portable vs. la futura UI SwiftUI;
- una decisión que conscientemente no implementaste por sobrearquitectura o falta de evidencia;
- respuestas breves a las preguntas de entrevista de la lección 17.

## Criterio de terminación

Tu solución está lista cuando cumple la rúbrica de la [evaluación final](../lessons/17-evaluacion-final.md), conserva los contratos relevantes y puedes explicar por qué tu diseño es suficiente para el problema actual.
