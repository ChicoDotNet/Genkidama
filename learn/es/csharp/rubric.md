# Rúbrica de evaluación final — C# / StockFlow

Puntuación máxima: **100 puntos**. La rúbrica mide evidencia observable, no cantidad de patrones ni tamaño del código.

## 1. Comprensión de arquitectura — 10 puntos

- 0–3: no distingue transporte, reglas y persistencia.
- 4–7: identifica responsabilidades principales con alguna confusión.
- 8–10: explica fronteras y trade-offs con precisión razonable para nivel junior.

## 2. Facturación simplificada — 25 puntos

- 0–8: la capacidad no funciona o mezcla responsabilidades de forma que impide probarla.
- 9–18: crea factura válida y maneja pedido inexistente, con detalles mejorables.
- 19–25: solución pequeña, clara, verificable y coherente con la arquitectura existente.

## 3. Bugfix de entrada nula — 15 puntos

- 0–4: el fallo permanece o sólo se oculta.
- 5–10: `sku: null` deja de producir excepción inesperada.
- 11–15: la causa se corrige en una frontera adecuada y queda protegida por prueba de regresión.

## 4. Pruebas — 20 puntos

- 0–6: pruebas ausentes, deshabilitadas o que no comprueban comportamiento relevante.
- 7–14: cubre camino feliz y errores principales.
- 15–20: elige niveles de prueba con criterio, nombres claros y regresiones útiles.

## 5. Contratos y manejo de errores — 10 puntos

- 0–3: statuses inconsistentes o detalles internos expuestos.
- 4–7: distingue validación, inexistencia y error inesperado.
- 8–10: contrato coherente con StockFlow y sin filtración innecesaria.

## 6. Uso de documentación oficial — 5 puntos

- 0: no hay evidencia.
- 1–3: cita una fuente oficial relevante.
- 4–5: explica qué decisión concreta ayudó a tomar.

## 7. Diseño de mejora futura — 10 puntos

- 0–3: propuesta genérica sin relación con el problema.
- 4–7: identifica la necesidad de consistencia/transacción.
- 8–10: explica atomicidad, frontera afectada y al menos un trade-off.

## 8. Explicación profesional — 5 puntos

- 0–1: exagera alcance o no puede explicar decisiones.
- 2–3: describe lo que construyó de forma entendible.
- 4–5: comunica problema, decisiones, pruebas, limitaciones y siguiente mejora sin inflar experiencia.

## Interpretación

- **0–59:** conviene volver a lecciones y resolver tareas con guía.
- **60–74:** fundamentos presentes, pero todavía hay huecos que conviene practicar antes de depender de esta habilidad en trabajo real.
- **75–89:** evidencia razonable para comenzar a intentar tareas u oportunidades junior con supervisión, mientras continúas practicando.
- **90–100:** evidencia sólida para el alcance de este curso; sigue siendo nivel inicial y no sustituye experiencia en equipos reales.

La puntuación no garantiza empleo ni equivale a una certificación profesional.
