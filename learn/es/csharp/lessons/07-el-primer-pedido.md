# Lección 07 — El primer pedido

## Qué vas a conseguir

StockFlow venderá por primera vez. Crearás un pedido con líneas, precios y total; si hay inventario suficiente, el stock se reducirá. Si falta un producto o existencias, el pedido completo será rechazado.

Aquí aparecen composición, records, clases con comportamiento y una dependencia real entre dos capacidades.

## El problema

Una API de inventario sin pedidos todavía no representa el flujo del negocio.

Queremos aceptar algo como:

```json
{
  "lines": [
    { "sku": "LAP-001", "quantity": 2 }
  ]
}
```

Y obtener un pedido cuyo total se calcula con el precio que StockFlow conoce, no con un número enviado por el cliente.

## Modelar la entrada

[`CreateOrderRequest.cs`](../app/src/StockFlow.Api/Orders/CreateOrderRequest.cs) contiene una lista de `OrderLineRequest`.

El cliente sólo decide:

- qué SKU quiere;
- cuántas unidades.

No permitimos que mande `UnitPrice` o `LineTotal`: esos datos pertenecen a las reglas del servidor.

## Modelar el resultado

Un `Order` contiene:

- identificador;
- fecha de creación;
- líneas;
- total.

Cada `OrderLine` conserva el SKU, nombre, cantidad, precio unitario y total de línea.

Esta es composición: un pedido **tiene** líneas. No necesitamos herencia para expresar esa relación.

## El servicio coordina, no posee todo

Abre [`OrderService.cs`](../app/src/StockFlow.Api/Orders/OrderService.cs).

Su constructor declara dos dependencias:

```csharp
public sealed class OrderService(ProductCatalog catalog, TimeProvider timeProvider)
```

El servicio necesita al catálogo para reservar productos y a un reloj para fechar el pedido.

`OrderService` no reimplementa cómo buscar ni descontar productos. Pide esa capacidad al objeto que ya gobierna inventario.

## Reserva antes de crear

```csharp
var reservation = catalog.TryReserve(
    request.Lines
        .Select(line => new StockRequest(line.Sku, line.Quantity))
        .ToArray());
```

Si falla la reserva, no existe pedido:

```csharp
if (!reservation.IsSuccess)
{
    return OrderCreationResult.Failure(reservation.Error!);
}
```

Cuando tiene éxito transformamos los productos reservados en líneas de pedido y calculamos:

```csharp
lines.Sum(line => line.LineTotal)
```

## ¿Por qué usar `TimeProvider`?

Podríamos escribir `DateTimeOffset.UtcNow`. Funcionaría.

Pero el reloj es una dependencia externa: el mismo código ejecutado un segundo después produce otro dato. `TimeProvider` nos permite usar el reloj real en producción y uno fijo en una prueba sin inventar una biblioteca propia.

## Endpoint

`POST /api/orders` sólo traduce HTTP a `OrderService.TryCreate` y convierte el resultado en `201 Created` o `400 Bad Request`.

Prueba:

```bash
curl -X POST http://localhost:5073/api/orders \
  -H "Content-Type: application/json" \
  -d '{"lines":[{"sku":"LAP-001","quantity":2}]}'
```

Luego consulta:

```bash
curl http://localhost:5073/api/products/LAP-001
curl http://localhost:5073/api/orders
```

Debes ver menor existencia y un pedido nuevo.

## Tu turno

Crea un pedido que solicite más laptops de las disponibles. Comprueba que:

1. recibes un error;
2. no aparece un pedido nuevo;
3. la existencia de `LAP-001` no cambia.

## Cómo comprobarlo

Ejecuta:

```bash
dotnet test app/tests/StockFlow.Api.Tests/StockFlow.Api.Tests.csproj
```

`OrderServiceTests` comprueba creación, total, reloj inyectado y descuento de inventario.

## Errores comunes

### Aceptar precio desde el request

Eso permitiría que el cliente decidiera cuánto cuesta el producto. El request debe contener sólo datos que realmente controla el consumidor.

### Descontar antes de validar todas las líneas

Puedes dejar el inventario en un estado parcial cuando una línea posterior falla. `TryReserve` valida primero el conjunto y modifica después.

### Crear una clase `GodService`

Que un servicio coordine dos capacidades no significa que deba poseer todas las reglas. Las reglas de catálogo siguen en el catálogo.

## Reflexión

Si mañana el catálogo se guarda en SQLite, ¿debería cambiar la forma de calcular `OrderLine.LineTotal`? Idealmente no. Persistencia y cálculo son razones distintas de cambio.

## Resumen

- los requests contienen decisiones del cliente, no datos que el servidor debe gobernar;
- composición representa naturalmente un pedido con líneas;
- un servicio puede coordinar otros componentes sin absorber todas sus reglas;
- una dependencia explícita es más fácil de sustituir en pruebas.

## Siguiente paso

En la [Lección 08](08-composicion-reglas-y-checkpoint.md) veremos por qué la reserva debe ser atómica y cerraremos este bloque con un checkpoint que obliga a modificar una regla sin receta.

## Referencias

- [Clases y objetos en C#](https://learn.microsoft.com/dotnet/csharp/fundamentals/types/classes)
- [TimeProvider](https://learn.microsoft.com/dotnet/standard/datetime/timeprovider-overview)
