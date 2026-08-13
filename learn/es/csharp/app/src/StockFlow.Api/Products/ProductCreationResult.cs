namespace StockFlow.Api.Products;

public sealed record ProductCreationResult(Product? Product, string? Error)
{
    public bool IsSuccess => Product is not null && Error is null;

    public static ProductCreationResult Success(Product product) => new(product, null);

    public static ProductCreationResult Failure(string error) => new(null, error);
}
