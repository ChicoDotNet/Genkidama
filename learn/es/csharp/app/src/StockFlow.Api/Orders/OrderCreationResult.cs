namespace StockFlow.Api.Orders;

public sealed record OrderCreationResult(bool IsSuccess, Order? Order, string? Error)
{
    public static OrderCreationResult Success(Order order) => new(true, order, null);

    public static OrderCreationResult Failure(string error) => new(false, null, error);
}
