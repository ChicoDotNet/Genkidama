using StockFlow.Api.Products;

namespace StockFlow.Api.Orders;

public sealed class OrderService(ProductCatalog catalog, TimeProvider timeProvider)
{
    private readonly object _gate = new();
    private readonly List<Order> _orders = [];

    public IReadOnlyList<Order> GetAll()
    {
        lock (_gate)
        {
            return _orders
                .OrderByDescending(order => order.CreatedAt)
                .ToArray();
        }
    }

    public OrderCreationResult TryCreate(CreateOrderRequest request)
    {
        if (request.Lines is null || request.Lines.Count == 0)
        {
            return OrderCreationResult.Failure("El pedido debe contener al menos una línea.");
        }

        var reservation = catalog.TryReserve(
            request.Lines
                .Select(line => new StockRequest(line.Sku, line.Quantity))
                .ToArray());

        if (!reservation.IsSuccess)
        {
            return OrderCreationResult.Failure(reservation.Error!);
        }

        var lines = reservation.Items
            .Select(item => new OrderLine(
                item.Sku,
                item.Name,
                item.Quantity,
                item.UnitPrice,
                item.UnitPrice * item.Quantity))
            .ToArray();

        var order = new Order(
            Guid.NewGuid(),
            timeProvider.GetUtcNow(),
            lines,
            lines.Sum(line => line.LineTotal));

        lock (_gate)
        {
            _orders.Add(order);
        }

        return OrderCreationResult.Success(order);
    }
}
