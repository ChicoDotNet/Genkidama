using Genkidama.Blueprints;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

[TestClass]
public sealed class BlueprintPlanRendererTests
{
    [TestMethod]
    public void Render_WithPlainText_ReturnsFile()
    {
        var item = new PlanItem("readme", "docs/readme.md", "Hello");
        var file = new PlanRenderer().Render(item, new PlanContext());
        Assert.AreEqual("docs/readme.md", file.RelativePath);
        Assert.AreEqual("Hello", file.Content);
    }
}
