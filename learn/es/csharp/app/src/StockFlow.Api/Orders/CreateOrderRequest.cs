namespace StockFlow.Api.Orders;

public sealed record CreateOrderRequest(IReadOnlyList<OrderLineRequest> Lines);
