# Lección 07 — Una implementación en memoria para aprender y probar

## Qué vas a conseguir

Usarás `InMemoryTimeQuoteRepository` como implementación pequeña del contrato de almacenamiento.

## El problema

Necesitamos comprobar la frontera antes de sumar I/O real. Meter archivos ahora mezclaría dos problemas: diseño del contrato y durabilidad.

## Concepto

El repositorio en memoria guarda una copia de `TimeQuoteBook`. Es útil para pruebas y para demostrar sustitución, pero no pretende sobrevivir cuando termina el proceso.

## Demostración

La prueba `servicePersistsChangesThroughRepositoryBoundary` crea el servicio, registra datos y consulta el resultado sin conocer el almacenamiento concreto.

```bash
swift test
```

## Tu turno

Crea un repositorio en memoria con un `TimeQuoteBook` que ya contenga un cliente y comprueba que un servicio nuevo pueda leer su resumen.

## Errores comunes

- Confundir "en memoria" con persistencia durable.
- Probar sólo métodos del repositorio y olvidar el comportamiento observable del servicio.
- Hacer público estado interno sólo para facilitar tests.

## Buenas prácticas

Usa dobles pequeños cuando permiten probar contratos reales sin introducir infraestructura innecesaria.

## Resumen

El contrato ya tiene una implementación sustituible y testeable.

## Siguiente paso

Continúa con [la lección 08](08-integracion-y-siguiente-persistencia.md).
