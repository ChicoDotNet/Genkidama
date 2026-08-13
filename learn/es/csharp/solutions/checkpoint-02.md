# Solución de referencia — Checkpoint 02

Una ubicación razonable es `OrderService.TryCreate`, porque el límite pertenece a la creación del pedido y no a la capacidad genérica del catálogo para reservar stock.

Antes de pedir la reserva, se puede detectar una línea fuera de rango:

```csharp
if (request.Lines.Any(line => line.Quantity > 10))
{
    return OrderCreationResult.Failure(
        "Una línea de pedido no puede superar 10 unidades.");
}
```

Una prueba útil debe demostrar dos cosas: rechazo y ausencia de efectos laterales.

```csharp
[TestMethod]
public void TryCreate_WithMoreThanTenUnitsInOneLine_DoesNotChangeInventory()
{
    var catalog = new ProductCatalog();
    var service = new OrderService(catalog, TimeProvider.System);
    var before = catalog.GetBySku("LAP-001")!.Stock;

    var result = service.TryCreate(new CreateOrderRequest(
    [
        new OrderLineRequest("LAP-001", 11)
    ]));

    Assert.IsFalse(result.IsSuccess);
    Assert.AreEqual(before, catalog.GetBySku("LAP-001")!.Stock);
}
```

## Por qué aquí

`ProductCatalog.TryReserve` puede ser útil en otros casos de uso donde reservar más de diez unidades sea válido. El límite descrito habla específicamente de **una línea de pedido**.

No existe una única respuesta universal: si el dominio definiera “ninguna operación de inventario puede mover más de diez unidades”, entonces la responsabilidad cambiaría. La ubicación depende del significado de la regla.
