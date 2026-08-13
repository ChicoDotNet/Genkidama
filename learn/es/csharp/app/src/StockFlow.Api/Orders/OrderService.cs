using StockFlow.Api.Products;

namespace StockFlow.Api.Orders;

public sealed class OrderService(
    ProductCatalog catalog,
    IOrderRepository repository,
    TimeProvider timeProvider)
{
    public Task<IReadOnlyList<Order>> GetAllAsync(CancellationToken cancellationToken = default) =>
        repository.GetAllAsync(cancellationToken);

    public async Task<OrderCreationResult> TryCreateAsync(
        CreateOrderRequest request,
        CancellationToken cancellationToken = default)
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

        try
        {
            await repository.AddAsync(order, cancellationToken);
        }
        catch
        {
            catalog.Restore(reservation.Items);
            throw;
        }

        return OrderCreationResult.Success(order);
    }
}
