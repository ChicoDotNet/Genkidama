# Lección 09 — Errores HTTP que forman parte del contrato

## Qué vas a conseguir

Vas a hacer que StockFlow responda de forma consistente cuando un recurso no existe o una petición viola una regla de negocio.

## Antes de empezar

Ejecuta las pruebas y levanta la API. Conserva una terminal para `curl`.

## El problema

Hasta ahora un error podía terminar como un objeto improvisado `{ error: ... }` o como una respuesta vacía. Para un cliente HTTP eso obliga a aprender formatos distintos.

## Concepto

HTTP ya tiene semántica útil: `200` para éxito, `201` para creación, `400` para una petición inválida y `404` cuando el recurso solicitado no existe. ASP.NET Core puede emitir `ProblemDetails`, un formato estándar para describir problemas HTTP.

## Demostración

[EJECUTAR]

```bash
curl -i http://localhost:5073/api/products/NO-EXISTE
```

La respuesta debe ser `404` con un título legible. Después intenta crear un producto inválido y observa el `400` con `title` y `detail`.

## Código real

En `Program.cs` los endpoints siguen siendo pequeños. La regla de negocio permanece en `ProductCatalog`; HTTP sólo traduce el resultado a un contrato de transporte.

## Qué acaba de pasar

Separar ambas responsabilidades permite probar reglas sin servidor y cambiar representación HTTP sin reescribir el dominio.

## Errores comunes

- devolver `200` con un texto que dice “error”;
- responder `500` por una validación del usuario;
- revelar stack traces o detalles internos;
- inventar un formato diferente en cada endpoint.

## Buenas prácticas

Usa códigos que representen lo ocurrido y mensajes suficientes para corregir la solicitud sin exponer internals.

## Tu turno

Prueba manualmente tres escenarios: producto inexistente, precio cero y pedido con stock insuficiente. Anota el código HTTP y el campo que permite entender el problema.

## Cómo comprobar

Los errores del usuario deben permanecer en la familia `4xx`; `/health` y consultas válidas deben seguir respondiendo correctamente.

## Reto adicional

Explica por qué un fallo al abrir SQLite más adelante no debería convertirse automáticamente en `400`.

## Resumen

HTTP también es parte del diseño público de una aplicación. Una API junior razonable no sólo “devuelve JSON”: comunica éxito y fallo de manera consistente.

## Siguiente paso

Ahora StockFlow ya tiene pedidos que vale la pena conservar. En la siguiente lección el reinicio del proceso dejará de borrar ese historial.

## Referencias

- [Problem Details en ASP.NET Core](https://learn.microsoft.com/aspnet/core/fundamentals/error-handling-api)
