# Lección 15 — Debugging, logging y diagnóstico

## Qué vas a conseguir

Vas a dejar evidencia útil de lo que hace StockFlow sin imprimir secretos, payloads completos ni mensajes inútiles. También practicarás una secuencia de diagnóstico antes de editar código a ciegas.

## El problema

Un cliente reporta: “mi pedido no entró”. Sin contexto, la tentación es reproducir al azar o agregar `Console.WriteLine` por todas partes.

## Concepto

`ILogger<T>` permite registrar eventos con nivel y propiedades estructuradas. El objetivo no es registrar todo: es dejar suficientes señales para reconstruir una decisión importante.

En StockFlow, `POST /api/orders` registra:

- `Warning` cuando una regla rechaza el pedido;
- `Information` cuando se crea correctamente;
- una excepción no controlada queda en manos del middleware de excepciones y del proveedor de logging configurado por ASP.NET Core.

## Demostración

[EJECUTAR]

Inicia la API y crea un pedido válido. Observa el identificador, total y número de líneas en el log.

Luego envía un pedido con existencia insuficiente y observa el warning.

## Código real

```csharp
logger.LogInformation(
    "Pedido {OrderId} creado por {Total} con {LineCount} líneas",
    result.Order.Id,
    result.Order.Total,
    result.Order.Lines.Count);
```

Las llaves no son interpolación de string: son nombres de propiedades que proveedores de logging pueden conservar estructuradamente.

## Qué acaba de pasar

Registramos identificadores y resultados, no el request completo. Eso reduce ruido y evita convertir el log en una copia accidental de datos de negocio.

## Secuencia de debugging

Cuando algo falla:

1. reproduce el comportamiento;
2. lee el error y los logs antes de cambiar código;
3. identifica la frontera que falló;
4. formula una hipótesis;
5. crea o mejora una prueba si el bug es reproducible;
6. cambia lo mínimo;
7. vuelve a ejecutar la prueba y la suite.

## Errores comunes

- `catch (Exception) { }` y seguir como si nada;
- registrar contraseñas, tokens o payloads completos;
- usar `Error` para eventos normales;
- logs como sustituto de tests;
- editar cinco capas antes de saber cuál falló.

## Buenas prácticas

Prefiere mensajes estables con propiedades, niveles coherentes y suficiente contexto para correlacionar un evento con la entidad afectada.

## Tu turno

Agrega un log de `Warning` para el intento de crear un producto inválido. Registra la razón, pero no serialices el request completo.

## Cómo comprobar

Provoca el error desde HTTP y confirma que la respuesta sigue siendo el mismo `ProblemDetails`. Logging no debe cambiar el contrato.

## Reflexión

¿Qué dato te gustaría tener en un incidente real y qué dato sería peligroso almacenar indefinidamente?

## Resumen

- logging es evidencia operacional, no decoración;
- niveles y propiedades importan;
- primero diagnostica, luego cambia;
- nunca registres secretos por comodidad.

## Siguiente paso

La última lección guiada endurece las fronteras antes de entregarte StockFlow para una evaluación autónoma.

## Referencias

- [Logging en .NET y ASP.NET Core](https://learn.microsoft.com/aspnet/core/fundamentals/logging/)
