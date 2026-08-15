namespace StockFlow.Api.Orders;

public sealed class InMemoryOrderRepository : IOrderRepository
{
    private readonly object _gate = new();
    private readonly List<Order> _orders = [];

    public Task InitializeAsync(CancellationToken cancellationToken = default) => Task.CompletedTask;

    public Task<IReadOnlyList<Order>> GetAllAsync(CancellationToken cancellationToken = default)
    {
        cancellationToken.ThrowIfCancellationRequested();

        lock (_gate)
        {
            IReadOnlyList<Order> result = _orders
                .OrderByDescending(order => order.CreatedAt)
                .ToArray();
            return Task.FromResult(result);
        }
    }

    public Task AddAsync(Order order, CancellationToken cancellationToken = default)
    {
        cancellationToken.ThrowIfCancellationRequested();

        lock (_gate)
        {
            _orders.Add(order);
        }

        return Task.CompletedTask;
    }
}
