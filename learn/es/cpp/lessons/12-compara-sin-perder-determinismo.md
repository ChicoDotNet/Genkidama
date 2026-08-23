# Lección 12 — Compara sin perder determinismo

## Objetivo

Comparar ejecución secuencial y paralela manteniendo el mismo contrato observable.

## Problema

La concurrencia puede cambiar el orden natural en que llegan resultados. Si el usuario recibe un orden distinto en cada ejecución, una optimización interna se convierte en una regresión visible.

## Concepto

Ambos caminos terminan ordenando los `FileRecord` por ruta. Las pruebas comparan secuencial y paralelo elemento por elemento.

El benchmark no forma parte del contrato: puede ganar uno u otro según el entorno. **La equivalencia funcional sí forma parte del contrato.**

## Aplicación real

Mide con diferentes cantidades de workers y documenta el punto donde la complejidad deja de aportar valor. En árboles pequeños, secuencial puede ser mejor; eso es un resultado válido.

## Errores comunes

- cambiar el orden de salida por paralelizar;
- medir sólo el caso favorable;
- ocultar que una optimización empeora cargas pequeñas;
- usar tiempos absolutos como gate de CI.

## Ejercicio

Agrega un árbol con al menos tres subdirectorios y demuestra que 1, 2 y 4 workers producen exactamente las mismas rutas y tamaños.

## Comprobación

`ctest --test-dir build --output-on-failure`

## Reflexión

¿Qué prefieres: una implementación 5% más rápida con comportamiento variable o una determinista y explicable? ¿Cuándo cambiaría tu respuesta?

## Siguiente paso

Completa el [Checkpoint 03](../exercises/checkpoint-03.md) y continúa con [Lección 13 — Cancela sin abandonar recursos](13-cancela-sin-abandonar-recursos.md).
