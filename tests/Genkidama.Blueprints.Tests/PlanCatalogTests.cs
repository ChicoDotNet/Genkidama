using Genkidama.Blueprints;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Blueprints.Tests;

[TestClass]
public sealed class PlanCatalogTests
{
    [TestMethod]
    public void Find_WithKnownId_ReturnsItem()
    {
        var item = new PlanItem("readme", "README.md", "Hello");
        var provider = new StaticPlanProvider("core", [item]);
        var catalog = new PlanCatalog([provider]);
        var found = catalog.Find("readme");
        Assert.AreEqual(item, found);
    }
}
