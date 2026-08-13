namespace StockFlow.Api.Products;

public sealed record ReservedProduct(
    Guid ProductId,
    string Sku,
    string Name,
    decimal UnitPrice,
    int Quantity);
