namespace StockFlow.Api.Products;

public sealed record CreateProductRequest(
    string Sku,
    string Name,
    decimal UnitPrice,
    int Stock);
