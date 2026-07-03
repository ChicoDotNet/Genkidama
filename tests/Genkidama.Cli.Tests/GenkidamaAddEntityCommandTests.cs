using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests the add entity command.
/// </summary>
[TestClass]
public sealed class GenkidamaAddEntityCommandTests
{
    /// <summary>
    /// Verifies that the command writes a domain entity file.
    /// </summary>
    [TestMethod]
    public async Task ExecuteAsync_WithOptions_WritesEntityFile()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        using var writer = new StringWriter();
        var options = new AddEntityOptions("SampleApp", "sales_order", root);
        var code = await GenkidamaAddEntityCommand.ExecuteAsync(options, writer);
        var path = Path.Combine(root, "src", "SampleApp.Domain", "Entities", "SalesOrder.cs");
        Assert.AreEqual(0, code);
        Assert.IsTrue(File.Exists(path));
        Assert.AreEqual("Added entity SalesOrder.", writer.ToString().Trim());
    }
}
