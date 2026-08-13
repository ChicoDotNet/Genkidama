using System.Globalization;
using System.Text.Json;
using Microsoft.Data.Sqlite;

namespace StockFlow.Api.Orders;

public sealed class SqliteOrderRepository(string connectionString) : IOrderRepository
{
    private static readonly JsonSerializerOptions JsonOptions = new(JsonSerializerDefaults.Web);

    public async Task InitializeAsync(CancellationToken cancellationToken = default)
    {
        await using var connection = new SqliteConnection(connectionString);
        await connection.OpenAsync(cancellationToken);

        var command = connection.CreateCommand();
        command.CommandText = """
            CREATE TABLE IF NOT EXISTS Orders (
                Id TEXT PRIMARY KEY,
                CreatedAt TEXT NOT NULL,
                LinesJson TEXT NOT NULL,
                Total TEXT NOT NULL
            );
            """;
        await command.ExecuteNonQueryAsync(cancellationToken);
    }

    public async Task<IReadOnlyList<Order>> GetAllAsync(CancellationToken cancellationToken = default)
    {
        await using var connection = new SqliteConnection(connectionString);
        await connection.OpenAsync(cancellationToken);

        var command = connection.CreateCommand();
        command.CommandText = "SELECT Id, CreatedAt, LinesJson, Total FROM Orders ORDER BY CreatedAt DESC;";

        var orders = new List<Order>();
        await using var reader = await command.ExecuteReaderAsync(cancellationToken);

        while (await reader.ReadAsync(cancellationToken))
        {
            var lines = JsonSerializer.Deserialize<OrderLine[]>(reader.GetString(2), JsonOptions)
                ?? throw new InvalidOperationException("Un pedido persistido contiene líneas inválidas.");

            orders.Add(new Order(
                Guid.Parse(reader.GetString(0)),
                DateTimeOffset.Parse(reader.GetString(1), CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind),
                lines,
                decimal.Parse(reader.GetString(3), CultureInfo.InvariantCulture)));
        }

        return orders;
    }

    public async Task AddAsync(Order order, CancellationToken cancellationToken = default)
    {
        await using var connection = new SqliteConnection(connectionString);
        await connection.OpenAsync(cancellationToken);

        var command = connection.CreateCommand();
        command.CommandText = """
            INSERT INTO Orders (Id, CreatedAt, LinesJson, Total)
            VALUES ($id, $createdAt, $linesJson, $total);
            """;
        command.Parameters.AddWithValue("$id", order.Id.ToString());
        command.Parameters.AddWithValue("$createdAt", order.CreatedAt.ToString("O", CultureInfo.InvariantCulture));
        command.Parameters.AddWithValue("$linesJson", JsonSerializer.Serialize(order.Lines, JsonOptions));
        command.Parameters.AddWithValue("$total", order.Total.ToString(CultureInfo.InvariantCulture));

        await command.ExecuteNonQueryAsync(cancellationToken);
    }
}
