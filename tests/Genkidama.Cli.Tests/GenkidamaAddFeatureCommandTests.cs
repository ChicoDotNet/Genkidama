using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests the add feature command.
/// </summary>
[TestClass]
public sealed class GenkidamaAddFeatureCommandTests
{
    /// <summary>
    /// Verifies that the command writes feature files.
    /// </summary>
    [TestMethod]
    public async Task ExecuteAsync_WithOptions_WritesFeatureFiles()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        using var writer = new StringWriter();
        var options = new AddFeatureOptions("SampleApp", "create_order", root);
        var code = await GenkidamaAddFeatureCommand.ExecuteAsync(options, writer);
        var path = Path.Combine(root, "src", "SampleApp.Application", "Features", "CreateOrder", "CreateOrderCommand.cs");
        Assert.AreEqual(0, code);
        Assert.IsTrue(File.Exists(path));
        Assert.AreEqual("Added feature CreateOrder.", writer.ToString().Trim());
    }
}
