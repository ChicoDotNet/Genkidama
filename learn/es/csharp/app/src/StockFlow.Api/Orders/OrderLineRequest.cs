namespace StockFlow.Api.Orders;

public sealed record OrderLineRequest(string Sku, int Quantity);
