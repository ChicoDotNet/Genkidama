using StockFlow.Api.Products;

var builder = WebApplication.CreateBuilder(args);
builder.Services.AddSingleton<ProductCatalog>();

var app = builder.Build();

app.MapGet("/health", () => Results.Ok(new { status = "ok" }));

app.MapGet("/api/products", (ProductCatalog catalog) =>
    Results.Ok(catalog.GetAll()));

app.MapPost("/api/products", (CreateProductRequest request, ProductCatalog catalog) =>
{
    var result = catalog.TryAdd(request);

    return result.IsSuccess
        ? Results.Created($"/api/products/{result.Product!.Id}", result.Product)
        : Results.BadRequest(new { error = result.Error });
});

app.Run();

public partial class Program
{
}
