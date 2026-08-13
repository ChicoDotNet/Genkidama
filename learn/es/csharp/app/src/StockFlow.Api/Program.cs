using StockFlow.Api.Orders;
using StockFlow.Api.Products;

var builder = WebApplication.CreateBuilder(args);

builder.WebHost.ConfigureKestrel(options =>
{
    options.AddServerHeader = false;
    options.Limits.MaxRequestBodySize = 1_048_576;
});

builder.Services.AddProblemDetails();
builder.Services.AddOpenApi();
builder.Services.AddSingleton<ProductCatalog>();
builder.Services.AddSingleton<TimeProvider>(TimeProvider.System);
builder.Services.AddSingleton<IOrderRepository>(_ =>
    new SqliteOrderRepository("Data Source=stockflow.db"));
builder.Services.AddSingleton<OrderService>();

var app = builder.Build();

app.UseExceptionHandler();

if (app.Environment.IsDevelopment())
{
    app.MapOpenApi();
}

var orderRepository = app.Services.GetRequiredService<IOrderRepository>();
await orderRepository.InitializeAsync();

app.MapGet("/health", () => Results.Ok(new { status = "ok" }))
    .WithName("Health")
    .WithSummary("Comprueba que StockFlow está disponible")
    .Produces(StatusCodes.Status200OK);

app.MapGet("/api/products", (string? search, int? maxStock, ProductCatalog catalog) =>
    Results.Ok(catalog.Search(search, maxStock)))
    .WithName("SearchProducts")
    .WithSummary("Busca productos por texto y existencia máxima")
    .Produces<IReadOnlyList<Product>>(StatusCodes.Status200OK);

app.MapGet("/api/products/{sku}", (string sku, ProductCatalog catalog) =>
{
    var product = catalog.GetBySku(sku);
    return product is null
        ? Results.Problem(statusCode: StatusCodes.Status404NotFound, title: "Producto no encontrado")
        : Results.Ok(product);
})
    .WithName("GetProductBySku")
    .WithSummary("Obtiene un producto por SKU")
    .Produces<Product>(StatusCodes.Status200OK)
    .ProducesProblem(StatusCodes.Status404NotFound);

app.MapPost("/api/products", (CreateProductRequest request, ProductCatalog catalog) =>
{
    var result = catalog.TryAdd(request);

    return result.IsSuccess
        ? Results.Created($"/api/products/{result.Product!.Sku}", result.Product)
        : Results.Problem(
            statusCode: StatusCodes.Status400BadRequest,
            title: "Producto inválido",
            detail: result.Error);
})
    .WithName("CreateProduct")
    .WithSummary("Agrega un producto al catálogo en memoria")
    .Produces<Product>(StatusCodes.Status201Created)
    .ProducesProblem(StatusCodes.Status400BadRequest);

app.MapGet("/api/orders", async (OrderService orders, CancellationToken cancellationToken) =>
    Results.Ok(await orders.GetAllAsync(cancellationToken)))
    .WithName("GetOrders")
    .WithSummary("Obtiene el historial persistido de pedidos")
    .Produces<IReadOnlyList<Order>>(StatusCodes.Status200OK);

app.MapPost("/api/orders", async (
    CreateOrderRequest request,
    OrderService orders,
    ILogger<Program> logger,
    CancellationToken cancellationToken) =>
{
    var result = await orders.TryCreateAsync(request, cancellationToken);

    if (!result.IsSuccess)
    {
        logger.LogWarning("Pedido rechazado: {Reason}", result.Error);
        return Results.Problem(
            statusCode: StatusCodes.Status400BadRequest,
            title: "Pedido inválido",
            detail: result.Error);
    }

    logger.LogInformation(
        "Pedido {OrderId} creado por {Total} con {LineCount} líneas",
        result.Order!.Id,
        result.Order.Total,
        result.Order.Lines.Count);

    return Results.Created($"/api/orders/{result.Order.Id}", result.Order);
})
    .WithName("CreateOrder")
    .WithSummary("Crea un pedido, descuenta inventario y persiste el resultado")
    .Produces<Order>(StatusCodes.Status201Created)
    .ProducesProblem(StatusCodes.Status400BadRequest)
    .ProducesProblem(StatusCodes.Status500InternalServerError);

app.Run();

public partial class Program
{
}
