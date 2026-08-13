using StockFlow.Api.Orders;
using StockFlow.Api.Products;

var builder = WebApplication.CreateBuilder(args);
builder.Services.AddSingleton<ProductCatalog>();
builder.Services.AddSingleton<TimeProvider>(TimeProvider.System);
builder.Services.AddSingleton<IOrderRepository>(_ =>
    new SqliteOrderRepository("Data Source=stockflow.db"));
builder.Services.AddSingleton<OrderService>();

var app = builder.Build();

var orderRepository = app.Services.GetRequiredService<IOrderRepository>();
await orderRepository.InitializeAsync();

app.MapGet("/health", () => Results.Ok(new { status = "ok" }));

app.MapGet("/api/products", (string? search, int? maxStock, ProductCatalog catalog) =>
    Results.Ok(catalog.Search(search, maxStock)));

app.MapGet("/api/products/{sku}", (string sku, ProductCatalog catalog) =>
{
    var product = catalog.GetBySku(sku);
    return product is null
        ? Results.Problem(statusCode: StatusCodes.Status404NotFound, title: "Producto no encontrado")
        : Results.Ok(product);
});

app.MapPost("/api/products", (CreateProductRequest request, ProductCatalog catalog) =>
{
    var result = catalog.TryAdd(request);

    return result.IsSuccess
        ? Results.Created($"/api/products/{result.Product!.Sku}", result.Product)
        : Results.Problem(
            statusCode: StatusCodes.Status400BadRequest,
            title: "Producto inválido",
            detail: result.Error);
});

app.MapGet("/api/orders", async (OrderService orders, CancellationToken cancellationToken) =>
    Results.Ok(await orders.GetAllAsync(cancellationToken)));

app.MapPost("/api/orders", async (
    CreateOrderRequest request,
    OrderService orders,
    CancellationToken cancellationToken) =>
{
    var result = await orders.TryCreateAsync(request, cancellationToken);

    return result.IsSuccess
        ? Results.Created($"/api/orders/{result.Order!.Id}", result.Order)
        : Results.Problem(
            statusCode: StatusCodes.Status400BadRequest,
            title: "Pedido inválido",
            detail: result.Error);
});

app.Run();

public partial class Program
{
}
