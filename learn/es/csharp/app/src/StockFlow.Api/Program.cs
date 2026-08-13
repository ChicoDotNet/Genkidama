using StockFlow.Api.Orders;
using StockFlow.Api.Products;

var builder = WebApplication.CreateBuilder(args);
builder.Services.AddSingleton<ProductCatalog>();
builder.Services.AddSingleton(TimeProvider.System);
builder.Services.AddSingleton<OrderService>();

var app = builder.Build();

app.MapGet("/health", () => Results.Ok(new { status = "ok" }));

app.MapGet("/api/products", (string? search, int? maxStock, ProductCatalog catalog) =>
    Results.Ok(catalog.Search(search, maxStock)));

app.MapGet("/api/products/{sku}", (string sku, ProductCatalog catalog) =>
{
    var product = catalog.GetBySku(sku);
    return product is null ? Results.NotFound() : Results.Ok(product);
});

app.MapPost("/api/products", (CreateProductRequest request, ProductCatalog catalog) =>
{
    var result = catalog.TryAdd(request);

    return result.IsSuccess
        ? Results.Created($"/api/products/{result.Product!.Sku}", result.Product)
        : Results.BadRequest(new { error = result.Error });
});

app.MapGet("/api/orders", (OrderService orders) => Results.Ok(orders.GetAll()));

app.MapPost("/api/orders", (CreateOrderRequest request, OrderService orders) =>
{
    var result = orders.TryCreate(request);

    return result.IsSuccess
        ? Results.Created($"/api/orders/{result.Order!.Id}", result.Order)
        : Results.BadRequest(new { error = result.Error });
});

app.Run();

public partial class Program
{
}
