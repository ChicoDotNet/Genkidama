using Genkidama.Blueprints;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Blueprints.Tests;

[TestClass]
public sealed class PlanRendererTests
{
    [TestMethod]
    public void Render_WithPlainText_ReturnsFile()
    {
        var item = new PlanItem("readme", "docs/readme.md", "Hello");
        var context = new PlanContext().With("Name", "Orders");
        var file = new PlanRenderer().Render(item, context);
        Assert.AreEqual("docs/readme.md", file.RelativePath);
        Assert.AreEqual("Hello", file.Content);
    }
}
