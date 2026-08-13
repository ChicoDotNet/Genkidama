using Microsoft.VisualStudio.TestTools.UnitTesting;
using StockFlow.Api.Orders;
using StockFlow.Api.Products;

namespace StockFlow.Api.Tests;

[TestClass]
public sealed class OrderServiceTests
{
    [TestMethod]
    public async Task TryCreateAsync_WithAvailableStock_CreatesOrderAndDecrementsInventory()
    {
        var catalog = new ProductCatalog();
        var repository = new InMemoryOrderRepository();
        var clock = new FixedTimeProvider(new DateTimeOffset(2026, 8, 12, 18, 0, 0, TimeSpan.Zero));
        var service = new OrderService(catalog, repository, clock);

        var result = await service.TryCreateAsync(new CreateOrderRequest(
        [
            new OrderLineRequest("LAP-001", 2)
        ]));

        Assert.IsTrue(result.IsSuccess);
        Assert.IsNotNull(result.Order);
        Assert.AreEqual(37000m, result.Order.Total);
        Assert.AreEqual(clock.GetUtcNow(), result.Order.CreatedAt);
        Assert.AreEqual(2, catalog.GetBySku("LAP-001")!.Stock);
        Assert.HasCount(1, await repository.GetAllAsync());
    }

    [TestMethod]
    public async Task TryCreateAsync_WithRepeatedSku_AggregatesQuantityIntoOneOrderLine()
    {
        var catalog = new ProductCatalog();
        var service = new OrderService(
            catalog,
            new InMemoryOrderRepository(),
            new FixedTimeProvider(DateTimeOffset.UnixEpoch));

        var result = await service.TryCreateAsync(new CreateOrderRequest(
        [
            new OrderLineRequest("LAP-001", 1),
            new OrderLineRequest("lap-001", 2)
        ]));

        Assert.IsTrue(result.IsSuccess);
        Assert.IsNotNull(result.Order);
        Assert.HasCount(1, result.Order.Lines);
        Assert.AreEqual(3, result.Order.Lines[0].Quantity);
        Assert.AreEqual(1, catalog.GetBySku("LAP-001")!.Stock);
    }

    [TestMethod]
    public async Task TryCreateAsync_WhenRepositoryFails_RestoresInventory()
    {
        var catalog = new ProductCatalog();
        var service = new OrderService(
            catalog,
            new FailingOrderRepository(),
            new FixedTimeProvider(DateTimeOffset.UnixEpoch));

        await Assert.ThrowsExactlyAsync<InvalidOperationException>(() =>
            service.TryCreateAsync(new CreateOrderRequest(
            [
                new OrderLineRequest("LAP-001", 2)
            ])));

        Assert.AreEqual(4, catalog.GetBySku("LAP-001")!.Stock);
    }

    private sealed class FixedTimeProvider(DateTimeOffset utcNow) : TimeProvider
    {
        public override DateTimeOffset GetUtcNow() => utcNow;
    }

    private sealed class FailingOrderRepository : IOrderRepository
    {
        public Task InitializeAsync(CancellationToken cancellationToken = default) => Task.CompletedTask;

        public Task<IReadOnlyList<Order>> GetAllAsync(CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<Order>>([]);

        public Task AddAsync(Order order, CancellationToken cancellationToken = default) =>
            Task.FromException(new InvalidOperationException("Fallo simulado de persistencia."));
    }
}
