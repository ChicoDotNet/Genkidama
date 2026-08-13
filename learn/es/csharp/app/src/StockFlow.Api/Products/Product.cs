namespace StockFlow.Api.Products;

public sealed record Product(
    Guid Id,
    string Sku,
    string Name,
    decimal UnitPrice,
    int Stock);
