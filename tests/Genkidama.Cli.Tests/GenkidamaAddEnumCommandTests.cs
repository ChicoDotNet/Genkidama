using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests the add enum command.
/// </summary>
[TestClass]
public sealed class GenkidamaAddEnumCommandTests
{
    /// <summary>
    /// Verifies that the command writes domain enum and seed files.
    /// </summary>
    [TestMethod]
    public async Task ExecuteAsync_WithOptions_WritesEnumFiles()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        using var writer = new StringWriter();
        var options = new AddEnumOptions("SampleApp", "order_status", ["draft", "submitted"], root);
        var code = await GenkidamaAddEnumCommand.ExecuteAsync(options, writer);
        var enumPath = Path.Combine(root, "src", "SampleApp.Domain", "Enums", "OrderStatus.cs");
        var seedPath = Path.Combine(root, "src", "SampleApp.Persistence", "Seeds", "Enums", "OrderStatus.json");
        Assert.AreEqual(0, code);
        Assert.IsTrue(File.Exists(enumPath));
        Assert.IsTrue(File.Exists(seedPath));
        Assert.AreEqual("Added enum OrderStatus.", writer.ToString().Trim());
    }
}
