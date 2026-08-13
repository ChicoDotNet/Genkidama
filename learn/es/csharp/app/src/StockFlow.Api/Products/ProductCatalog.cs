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

    public IReadOnlyList<Product> GetAll() => Search(null, null);

    public IReadOnlyList<Product> Search(string? text, int? maxStock)
    {
        lock (_gate)
        {
            IEnumerable<Product> query = _products;

            if (!string.IsNullOrWhiteSpace(text))
            {
                var normalizedText = text.Trim();
                query = query.Where(product =>
                    product.Sku.Contains(normalizedText, StringComparison.OrdinalIgnoreCase) ||
                    product.Name.Contains(normalizedText, StringComparison.OrdinalIgnoreCase));
            }

            if (maxStock is not null)
            {
                query = query.Where(product => product.Stock <= maxStock.Value);
            }

            return query
                .OrderBy(product => product.Name)
                .ThenBy(product => product.Sku)
                .ToArray();
        }
    }

    public Product? GetBySku(string sku)
    {
        if (string.IsNullOrWhiteSpace(sku))
        {
            return null;
        }

        var normalizedSku = sku.Trim();

        lock (_gate)
        {
            return _products.FirstOrDefault(product =>
                string.Equals(product.Sku, normalizedSku, StringComparison.OrdinalIgnoreCase));
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

    public StockReservationResult TryReserve(IReadOnlyList<StockRequest> requests)
    {
        if (requests.Count == 0)
        {
            return StockReservationResult.Failure("El pedido debe contener al menos una línea.");
        }

        if (requests.Any(request => request.Quantity <= 0))
        {
            return StockReservationResult.Failure("Todas las cantidades deben ser mayores que cero.");
        }

        var normalizedRequests = requests
            .GroupBy(request => request.Sku.Trim(), StringComparer.OrdinalIgnoreCase)
            .Select(group => new StockRequest(group.Key.ToUpperInvariant(), group.Sum(item => item.Quantity)))
            .ToArray();

        lock (_gate)
        {
            foreach (var request in normalizedRequests)
            {
                var product = _products.FirstOrDefault(candidate =>
                    string.Equals(candidate.Sku, request.Sku, StringComparison.OrdinalIgnoreCase));

                if (product is null)
                {
                    return StockReservationResult.Failure($"No existe el producto {request.Sku}.");
                }

                if (product.Stock < request.Quantity)
                {
                    return StockReservationResult.Failure(
                        $"Stock insuficiente para {product.Sku}. Disponible: {product.Stock}.");
                }
            }

            var reserved = new List<ReservedProduct>(normalizedRequests.Length);

            foreach (var request in normalizedRequests)
            {
                var index = _products.FindIndex(candidate =>
                    string.Equals(candidate.Sku, request.Sku, StringComparison.OrdinalIgnoreCase));
                var product = _products[index];

                _products[index] = product with { Stock = product.Stock - request.Quantity };
                reserved.Add(new ReservedProduct(
                    product.Id,
                    product.Sku,
                    product.Name,
                    product.UnitPrice,
                    request.Quantity));
            }

            return StockReservationResult.Success(reserved);
        }
    }

    public void Restore(IReadOnlyList<ReservedProduct> reservedProducts)
    {
        lock (_gate)
        {
            foreach (var reserved in reservedProducts)
            {
                var index = _products.FindIndex(product => product.Id == reserved.ProductId);
                if (index < 0)
                {
                    throw new InvalidOperationException($"No se puede restaurar el producto {reserved.Sku}.");
                }

                var product = _products[index];
                _products[index] = product with { Stock = product.Stock + reserved.Quantity };
            }
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
