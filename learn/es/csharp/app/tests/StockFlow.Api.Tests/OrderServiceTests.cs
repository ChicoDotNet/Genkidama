using Microsoft.VisualStudio.TestTools.UnitTesting;
using StockFlow.Api.Orders;
using StockFlow.Api.Products;

namespace StockFlow.Api.Tests;

[TestClass]
public sealed class OrderServiceTests
{
    [TestMethod]
    public void TryCreate_WithAvailableStock_CreatesOrderAndDecrementsInventory()
    {
        var catalog = new ProductCatalog();
        var clock = new FixedTimeProvider(new DateTimeOffset(2026, 8, 12, 18, 0, 0, TimeSpan.Zero));
        var service = new OrderService(catalog, clock);

        var result = service.TryCreate(new CreateOrderRequest(
        [
            new OrderLineRequest("LAP-001", 2)
        ]));

        Assert.IsTrue(result.IsSuccess);
        Assert.IsNotNull(result.Order);
        Assert.AreEqual(37000m, result.Order.Total);
        Assert.AreEqual(clock.GetUtcNow(), result.Order.CreatedAt);
        Assert.AreEqual(2, catalog.GetBySku("LAP-001")!.Stock);
    }

    [TestMethod]
    public void TryCreate_WithRepeatedSku_AggregatesQuantityIntoOneOrderLine()
    {
        var catalog = new ProductCatalog();
        var service = new OrderService(catalog, new FixedTimeProvider(DateTimeOffset.UnixEpoch));

        var result = service.TryCreate(new CreateOrderRequest(
        [
            new OrderLineRequest("LAP-001", 1),
            new OrderLineRequest("lap-001", 2)
        ]));

        Assert.IsTrue(result.IsSuccess);
        Assert.IsNotNull(result.Order);
        Assert.AreEqual(1, result.Order.Lines.Count);
        Assert.AreEqual(3, result.Order.Lines[0].Quantity);
        Assert.AreEqual(1, catalog.GetBySku("LAP-001")!.Stock);
    }

    private sealed class FixedTimeProvider(DateTimeOffset utcNow) : TimeProvider
    {
        public override DateTimeOffset GetUtcNow() => utcNow;
    }
}
