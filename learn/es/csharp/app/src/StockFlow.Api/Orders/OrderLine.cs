namespace StockFlow.Api.Orders;

public sealed record OrderLine(
    string Sku,
    string ProductName,
    int Quantity,
    decimal UnitPrice,
    decimal LineTotal);
