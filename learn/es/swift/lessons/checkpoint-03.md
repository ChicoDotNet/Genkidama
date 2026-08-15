# Checkpoint 03 — Haz durable TimeQuote

## Objetivo

Demostrar que puedes sustituir almacenamiento en memoria por persistencia JSON sin cambiar el dominio ni los casos de uso.

## Misión

1. Crea un archivo temporal para TimeQuote.
2. Construye un `TimeQuoteService` con `FileTimeQuoteRepository`.
3. Agrega un cliente y al menos dos registros de tiempo.
4. Descarta esa instancia.
5. Crea un servicio nuevo con un repositorio nuevo apuntando al mismo archivo.
6. Comprueba que minutos e importe se reconstruyen correctamente.
7. Corrompe una copia del archivo y comprueba que aparece un error de persistencia explícito.

## Restricciones

- No cambies `TimeQuoteService` para que conozca JSON o rutas.
- No uses variables globales para compartir el estado entre las dos instancias.
- No conviertas un archivo corrupto en un libro vacío.
- Limpia archivos temporales al terminar.

## Cómo comprobar

[EJECUTAR]

```bash
cd app
swift test
```

El checkpoint está terminado cuando la prueba demuestra supervivencia entre instancias y el caso corrupto falla con semántica explícita.

## Reflexión

¿Por qué una prueba de recreación aporta evidencia más fuerte que verificar que `save` fue llamado?

## Solución

Revisa la implementación y las pruebas de referencia en:

- [`Repository.swift`](../app/Sources/TimeQuote/Repository.swift)
- [`TimeQuoteBookTests.swift`](../app/Tests/TimeQuoteTests/TimeQuoteBookTests.swift)

## Siguiente paso

Continúa con [la lección 13 — Haz explícito el trabajo asíncrono](13-trabajo-asincrono.md) cuando puedas explicar qué partes de TimeQuote pertenecen al dominio y cuáles son infraestructura reemplazable.
