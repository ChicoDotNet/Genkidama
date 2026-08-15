using Microsoft.VisualStudio.TestTools.UnitTesting;
using StockFlow.Api.Products;

namespace StockFlow.Api.Tests;

[TestClass]
public sealed class ProductCatalogTests
{
    [TestMethod]
    public void TryAdd_WithValidProduct_AddsNormalizedProduct()
    {
        var catalog = new ProductCatalog();

        var result = catalog.TryAdd(new CreateProductRequest("  mouse-01 ", " Mouse ", 450m, 8));

        Assert.IsTrue(result.IsSuccess);
        Assert.IsNotNull(result.Product);
        Assert.AreEqual("MOUSE-01", result.Product.Sku);
        Assert.AreEqual("Mouse", result.Product.Name);
    }

    [TestMethod]
    public void TryAdd_WithNegativeStock_ReturnsValidationError()
    {
        var catalog = new ProductCatalog();

        var result = catalog.TryAdd(new CreateProductRequest("KB-01", "Teclado", 900m, -1));

        Assert.IsFalse(result.IsSuccess);
        Assert.AreEqual("La existencia no puede ser negativa.", result.Error);
    }

    [TestMethod]
    public void TryAdd_WithDuplicateSku_ReturnsConflictMessage()
    {
        var catalog = new ProductCatalog();
        catalog.TryAdd(new CreateProductRequest("MON-01", "Monitor", 4200m, 2));

        var duplicate = catalog.TryAdd(new CreateProductRequest("mon-01", "Otro monitor", 4000m, 1));

        Assert.IsFalse(duplicate.IsSuccess);
        Assert.AreEqual("Ya existe un producto con ese SKU.", duplicate.Error);
    }

    [TestMethod]
    public void Search_WithTextAndStockFilter_ReturnsOnlyMatchingProducts()
    {
        var catalog = new ProductCatalog();
        catalog.TryAdd(new CreateProductRequest("MOU-01", "Mouse inalámbrico", 450m, 3));
        catalog.TryAdd(new CreateProductRequest("MOU-02", "Mouse ergonómico", 650m, 12));

        var results = catalog.Search("mouse", 5);

        Assert.AreEqual(1, results.Count);
        Assert.AreEqual("MOU-01", results[0].Sku);
    }

    [TestMethod]
    public void TryReserve_WhenAnyLineHasInsufficientStock_DoesNotChangeAnyProduct()
    {
        var catalog = new ProductCatalog();
        catalog.TryAdd(new CreateProductRequest("MOU-01", "Mouse", 450m, 3));

        var result = catalog.TryReserve(
        [
            new StockRequest("MOU-01", 2),
            new StockRequest("LAP-001", 10)
        ]);

        Assert.IsFalse(result.IsSuccess);
        Assert.AreEqual(3, catalog.GetBySku("MOU-01")!.Stock);
        Assert.AreEqual(4, catalog.GetBySku("LAP-001")!.Stock);
    }
}
