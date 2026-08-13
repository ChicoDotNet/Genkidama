using System.Net;
using System.Net.Http.Json;
using System.Text.Json;
using Microsoft.AspNetCore.Mvc;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using StockFlow.Api.Orders;
using StockFlow.Api.Products;

namespace StockFlow.Api.Tests;

[TestClass]
public sealed class EndpointTests
{
    [TestMethod]
    public async Task GetProducts_ReturnsSeedProduct()
    {
        await using var factory = new StockFlowApiFactory();
        using var client = factory.CreateClient();

        var products = await client.GetFromJsonAsync<List<Product>>("/api/products");

        Assert.IsNotNull(products);
        Assert.IsTrue(products.Any(product => product.Sku == "LAP-001"));
    }

    [TestMethod]
    public async Task PostInvalidProduct_ReturnsProblemDetails()
    {
        await using var factory = new StockFlowApiFactory();
        using var client = factory.CreateClient();

        var response = await client.PostAsJsonAsync(
            "/api/products",
            new CreateProductRequest("", "Mouse", 450m, 1));
        var problem = await response.Content.ReadFromJsonAsync<ProblemDetails>();

        Assert.AreEqual(HttpStatusCode.BadRequest, response.StatusCode);
        Assert.IsNotNull(problem);
        Assert.AreEqual("Producto inválido", problem.Title);
        StringAssert.Contains(problem.Detail, "SKU");
    }

    [TestMethod]
    public async Task PostOrder_ThenGetOrders_ReturnsPersistedOrder()
    {
        await using var factory = new StockFlowApiFactory();
        using var client = factory.CreateClient();

        var createResponse = await client.PostAsJsonAsync(
            "/api/orders",
            new CreateOrderRequest(
            [
                new OrderLineRequest("LAP-001", 1)
            ]));
        var created = await createResponse.Content.ReadFromJsonAsync<Order>();
        var orders = await client.GetFromJsonAsync<List<Order>>("/api/orders");

        Assert.AreEqual(HttpStatusCode.Created, createResponse.StatusCode);
        Assert.IsNotNull(created);
        Assert.IsNotNull(orders);
        Assert.IsTrue(orders.Any(order => order.Id == created.Id));
    }

    [TestMethod]
    public async Task OpenApi_InDevelopment_DocumentsOrderEndpoint()
    {
        await using var factory = new StockFlowApiFactory();
        using var client = factory.CreateClient();

        var response = await client.GetAsync("/openapi/v1.json");
        var json = await response.Content.ReadAsStringAsync();
        using var document = JsonDocument.Parse(json);

        Assert.AreEqual(HttpStatusCode.OK, response.StatusCode);
        Assert.IsTrue(document.RootElement.GetProperty("paths").TryGetProperty("/api/orders", out _));
    }
}
