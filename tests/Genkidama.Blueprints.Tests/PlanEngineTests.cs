using Genkidama.Blueprints;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Blueprints.Tests;

[TestClass]
public sealed class PlanEngineTests
{
    [TestMethod]
    public void Render_WithKnownId_ReturnsFile()
    {
        var item = new PlanItem("readme", "README.md", "Hello");
        var catalog = new PlanCatalog([new StaticPlanProvider("core", [item])]);
        var engine = new PlanEngine(catalog, new PlanRenderer());
        var file = engine.Render("readme", new PlanContext());
        Assert.IsNotNull(file);
        Assert.AreEqual("README.md", file.RelativePath);
    }
}
