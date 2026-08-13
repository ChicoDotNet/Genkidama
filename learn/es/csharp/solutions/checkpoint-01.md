# Solución de referencia — Checkpoint 01

Esta es una solución posible, no la única forma válida.

## Regla

En `ProductCatalog.Validate`, después de comprobar que el stock no sea negativo:

```csharp
if (request.Stock > 10_000)
{
    return "La existencia inicial no puede superar 10,000 unidades.";
}
```

La regla queda en el catálogo porque debe aplicarse independientemente de que el alta llegue hoy por HTTP y mañana por otro adaptador.

## Prueba

Agrega un escenario semejante a:

```csharp
[TestMethod]
public void TryAdd_WithExcessiveInitialStock_ReturnsValidationError()
{
    var catalog = new ProductCatalog();

    var result = catalog.TryAdd(
        new CreateProductRequest("BULK-01", "Producto masivo", 10m, 10_001));

    Assert.IsFalse(result.IsSuccess);
    Assert.AreEqual("La existencia inicial no puede superar 10,000 unidades.", result.Error);
}
```

## Comprobación

Ejecuta toda la suite, no sólo la prueba nueva:

```bash
dotnet test app/tests/StockFlow.Api.Tests/StockFlow.Api.Tests.csproj
```

Después verifica el endpoint con una petición cuyo `stock` sea `10001`.

## Qué evaluar en tu propia solución

- ¿la regla está expresada una sola vez?;
- ¿el mensaje es accionable?;
- ¿la prueba falla si eliminas deliberadamente la regla?;
- ¿las reglas anteriores siguen protegidas?;
- ¿evitaste introducir una abstracción innecesaria para una condición sencilla?
