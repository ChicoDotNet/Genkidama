using Genkidama.Blueprints;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

[TestClass]
public sealed class BlueprintPlanCatalogTests
{
    [TestMethod]
    public void Find_WithKnownId_ReturnsItem()
    {
        var item = new PlanItem("one", "one.txt", "1");
        var provider = new StaticPlanProvider("core", [item]);
        var catalog = new PlanCatalog([provider]);
        Assert.IsNotNull(catalog.Find("one"));
    }
}
