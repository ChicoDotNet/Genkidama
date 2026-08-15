# Checkpoint 02 — Sustituye almacenamiento sin tocar el dominio

## Objetivo

Comprobar que puedes razonar sobre protocolos, casos de uso y sustitución sin copiar una implementación completa.

## Misión

1. crea un `TimeQuoteBook` inicial con un cliente;
2. úsalo para construir `InMemoryTimeQuoteRepository`;
3. crea `TimeQuoteService` con ese repositorio;
4. registra una entrada adicional mediante el servicio;
5. comprueba el resumen con una prueba;
6. explica qué archivo cambiarías para agregar persistencia durable y qué archivos no deberían cambiar.

[PAUSA PARA EJERCICIO]

No modifiques visibilidad privada para hacer más fácil la prueba.

## Evidencia

```bash
swift test
swift run TimeQuote
```

Debes poder explicar la dirección de dependencias y por qué el protocolo pertenece a la necesidad de la aplicación, no al mecanismo de disco.

## Reto adicional

Diseña, sin implementarlo todavía, un `FileTimeQuoteRepository`. Enumera al menos dos fallos de I/O que debería propagar o traducir de forma explícita.

## Solución

Después de tu intento, compara con [la solución de referencia](../solutions/checkpoint-02.md).

## Siguiente paso

Continúa con la lección 09 cuando esté disponible: persistencia durable detrás del mismo contrato.
