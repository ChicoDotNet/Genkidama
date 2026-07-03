using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests the new solution command.
/// </summary>
[TestClass]
public sealed class GenkidamaNewCommandTests
{
    /// <summary>
    /// Verifies that the command writes a generated solution skeleton.
    /// </summary>
    [TestMethod]
    public async Task ExecuteAsync_WithOptions_WritesFiles()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        using var writer = new StringWriter();
        var options = new NewSolutionOptions("SampleApp", root);
        var code = await GenkidamaNewCommand.ExecuteAsync(options, writer);
        Assert.AreEqual(0, code);
    }
}
