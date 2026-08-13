namespace StockFlow.Api.Orders;

public sealed record Order(
    Guid Id,
    DateTimeOffset CreatedAt,
    IReadOnlyList<OrderLine> Lines,
    decimal Total);
