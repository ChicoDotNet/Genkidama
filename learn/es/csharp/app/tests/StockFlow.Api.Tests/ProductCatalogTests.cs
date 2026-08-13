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
}
