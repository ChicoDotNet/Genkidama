namespace StockFlow.Api.Products;

public sealed record StockReservationResult(
    bool IsSuccess,
    IReadOnlyList<ReservedProduct> Items,
    string? Error)
{
    public static StockReservationResult Success(IReadOnlyList<ReservedProduct> items) =>
        new(true, items, null);

    public static StockReservationResult Failure(string error) =>
        new(false, Array.Empty<ReservedProduct>(), error);
}
