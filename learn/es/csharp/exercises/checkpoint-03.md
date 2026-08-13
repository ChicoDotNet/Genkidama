# Checkpoint 03 — Consultar un pedido por id

StockFlow persiste y lista pedidos, pero un cliente no puede recuperar uno concreto.

Implementa `GET /api/orders/{id}` sin seguir una receta paso a paso.

## Criterios de aceptación

- amplía `IOrderRepository` con una operación asíncrona para buscar por `Guid`;
- implementa el contrato tanto en memoria como en SQLite;
- expón la capacidad desde `OrderService`;
- devuelve `200` con el pedido cuando existe;
- devuelve `404` con un `ProblemDetails` consistente cuando no existe;
- propaga `CancellationToken` hasta SQLite;
- agrega al menos una prueba que proteja el comportamiento;
- no dupliques la lógica de reconstrucción de pedidos innecesariamente.

## Comprobación

```bash
dotnet test app/tests/StockFlow.Api.Tests/StockFlow.Api.Tests.csproj
```

Después crea un pedido, copia su `id` y consulta el nuevo endpoint.

Intenta primero sin abrir la [solución de referencia](../solutions/checkpoint-03.md).
