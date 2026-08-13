namespace StockFlow.Api.Products;

public sealed class ProductCatalog
{
    private readonly object _gate = new();
    private readonly List<Product> _products =
    [
        new Product(
            Guid.Parse("8fdc85fb-55c2-4fb4-bdde-9bcfb922c7a1"),
            "LAP-001",
            "Laptop de demostración",
            18500m,
            4)
    ];

    public IReadOnlyList<Product> GetAll()
    {
        lock (_gate)
        {
            return [.. _products];
        }
    }

    public ProductCreationResult TryAdd(CreateProductRequest request)
    {
        var error = Validate(request);
        if (error is not null)
        {
            return ProductCreationResult.Failure(error);
        }

        var normalizedSku = request.Sku.Trim().ToUpperInvariant();

        lock (_gate)
        {
            if (_products.Any(product =>
                    string.Equals(product.Sku, normalizedSku, StringComparison.OrdinalIgnoreCase)))
            {
                return ProductCreationResult.Failure("Ya existe un producto con ese SKU.");
            }

            var product = new Product(
                Guid.NewGuid(),
                normalizedSku,
                request.Name.Trim(),
                request.UnitPrice,
                request.Stock);

            _products.Add(product);
            return ProductCreationResult.Success(product);
        }
    }

    private static string? Validate(CreateProductRequest request)
    {
        if (string.IsNullOrWhiteSpace(request.Sku))
        {
            return "El SKU es obligatorio.";
        }

        if (string.IsNullOrWhiteSpace(request.Name))
        {
            return "El nombre es obligatorio.";
        }

        if (request.UnitPrice <= 0)
        {
            return "El precio debe ser mayor que cero.";
        }

        if (request.Stock < 0)
        {
            return "La existencia no puede ser negativa.";
        }

        return null;
    }
}
