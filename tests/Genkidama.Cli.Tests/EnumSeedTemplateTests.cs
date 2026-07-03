using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests enum seed template generation.
/// </summary>
[TestClass]
public sealed class EnumSeedTemplateTests
{
    /// <summary>
    /// Verifies that the template generates the expected persistence seed file.
    /// </summary>
    [TestMethod]
    public void Create_WithValues_ReturnsSeedFile()
    {
        var file = EnumSeedTemplate.Create("SampleApp", "order_status", ["draft", "submitted"]);
        Assert.AreEqual("src/SampleApp.Persistence/Seeds/Enums/OrderStatus.json", file.RelativePath);
        StringAssert.Contains(file.Content, "\"schema\": \"enum\"");
        StringAssert.Contains(file.Content, "\"name\": \"Submitted\"");
    }
}
