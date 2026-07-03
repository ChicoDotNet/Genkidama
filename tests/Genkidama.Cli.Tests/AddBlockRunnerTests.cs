using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests block file generation.
/// </summary>
[TestClass]
public sealed class AddBlockRunnerTests
{
    /// <summary>
    /// Verifies that the runner writes block files.
    /// </summary>
    [TestMethod]
    public async Task RunAsync_WithOptions_WritesBlockFiles()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        using var writer = new StringWriter();
        var options = new AddComponentOptions("SampleApp", "audit", root);
        var code = await AddBlockRunner.RunAsync(options, writer);
        var path = Path.Combine(root, "src", "SampleApp.Application", "Blocks", "Audit", "AuditStarter.cs");
        Assert.AreEqual(0, code);
        Assert.IsTrue(File.Exists(path));
        Assert.AreEqual("Added block Audit.", writer.ToString().Trim());
    }
}
