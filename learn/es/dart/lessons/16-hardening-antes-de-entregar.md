# Lección 16 — Hardening antes de entregar

## Qué vas a conseguir
Harás una revisión de entrega de PocketLedger para distinguir código funcional de una aplicación razonablemente mantenible.

## El problema
Una app puede funcionar en la laptop del autor y aun así ser frágil: datos corruptos, errores sin contexto, dependencias implícitas, builds no reproducibles o diagnósticos que filtran información.

## Concepto
Antes de considerar PocketLedger listo para evaluación final revisa estas fronteras:

- dominio: dinero entero, fechas y categorías válidas;
- persistencia: versión explícita, errores visibles y escritura segura;
- estado: persist-before-publish;
- privacidad: diagnósticos sin texto introducido por el usuario;
- calidad: format + analyze + tests;
- entrega: build reproducible con versión de Flutter fijada;
- recuperación: un archivo inválido nunca se borra automáticamente para “arreglar” el arranque.

## Demostración
[DEMO] Toma un fallo de persistencia y sigue el camino completo hasta la UI. Después toma un gasto válido y verifica que el mismo dato aparece en reporte, filtro y persistencia sin fuentes paralelas.

## Errores comunes
- Silenciar un archivo corrupto y empezar vacío.
- Añadir `try/catch` que pierde la causa original.
- Registrar descripciones de gastos en diagnósticos.
- Declarar soporte móvil sólo porque `flutter test` pasó.
- Perseguir 100% de coverage en lugar de proteger contratos y regresiones reales.

## Tu turno
Ejecuta el Checkpoint 04. Tu objetivo es producir una evidencia de diagnóstico segura, una prueba de regresión y el pipeline de calidad completo.

[PAUSA PARA EJERCICIO]

[Checkpoint 04 — diagnostica y endurece PocketLedger](../exercises/checkpoint-04.md)

## Resumen
Hardening es convertir supuestos importantes en contratos, pruebas o documentación verificable. No es añadir complejidad decorativa.

## Siguiente paso
Continúa con la [Lección 17 — evaluación final: entrega PocketLedger](17-evaluacion-final.md).

## Referencias
- [Flutter testing](https://docs.flutter.dev/testing/overview)
- [Dart effective usage](https://dart.dev/effective-dart/usage)
