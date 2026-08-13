# Checkpoint 04 — Correlaciona una petición sin filtrar datos

## Situación

Soporte recibe el identificador de un pedido, pero en una sesión con varias solicitudes es difícil saber qué logs pertenecen a la misma petición HTTP.

## Tu trabajo

Implementa un identificador de correlación simple para StockFlow:

- acepta opcionalmente un header `X-Request-Id`;
- si no llega, genera un identificador seguro para trazabilidad;
- devuelve el identificador en `X-Request-Id` de la respuesta;
- úsalo como propiedad de contexto para que los logs producidos durante esa petición puedan correlacionarse;
- no confíes ciegamente en un valor de tamaño arbitrario enviado por el cliente;
- agrega al menos una prueba HTTP que demuestre el comportamiento.

No se especifica en qué archivo ni qué API exacta debes usar. Investiga las opciones oficiales de ASP.NET Core y elige una implementación pequeña.

## Criterios de aceptación

1. una petición sin header recibe uno en la respuesta;
2. una petición con un identificador razonable puede conservarlo;
3. un valor excesivo no se copia sin límite;
4. las pruebas existentes siguen verdes;
5. puedes explicar por qué el identificador no es un secreto ni una credencial.

## Antes de ver la solución

Escribe en dos frases dónde colocaste esta responsabilidad y por qué es una preocupación HTTP transversal en lugar de una regla de `OrderService`.
