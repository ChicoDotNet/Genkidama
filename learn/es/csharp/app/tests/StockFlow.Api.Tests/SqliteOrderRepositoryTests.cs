using Microsoft.VisualStudio.TestTools.UnitTesting;
using StockFlow.Api.Orders;

namespace StockFlow.Api.Tests;

[TestClass]
public sealed class SqliteOrderRepositoryTests
{
    [TestMethod]
    public async Task AddAsync_ThenGetAllAsync_PersistsOrderAcrossRepositoryInstances()
    {
        var databasePath = Path.Combine(Path.GetTempPath(), $"stockflow-{Guid.NewGuid():N}.db");
        var connectionString = $"Data Source={databasePath}";

        try
        {
            var writer = new SqliteOrderRepository(connectionString);
            await writer.InitializeAsync();

            var expected = new Order(
                Guid.NewGuid(),
                new DateTimeOffset(2026, 8, 12, 20, 0, 0, TimeSpan.Zero),
                [new OrderLine("LAP-001", "Laptop", 1, 18500m, 18500m)],
                18500m);

            await writer.AddAsync(expected);

            var reader = new SqliteOrderRepository(connectionString);
            await reader.InitializeAsync();
            var orders = await reader.GetAllAsync();

            Assert.HasCount(1, orders);
            var actual = orders[0];
            Assert.AreEqual(expected.Id, actual.Id);
            Assert.AreEqual(expected.CreatedAt, actual.CreatedAt);
            Assert.AreEqual(expected.Total, actual.Total);
            Assert.HasCount(1, actual.Lines);
            Assert.AreEqual(expected.Lines[0], actual.Lines[0]);
        }
        finally
        {
            File.Delete(databasePath);
        }
    }
}
