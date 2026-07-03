using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests add feature routing.
/// </summary>
[TestClass]
public sealed class GenkidamaCommandRouterAddFeatureTests
{
    /// <summary>
    /// Verifies that add feature uses the current directory name as the app name.
    /// </summary>
    [TestMethod]
    public async Task ExecuteAsync_WithAddFeature_WritesFeatureInCurrentApp()
    {
        var original = Environment.CurrentDirectory;
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"), "SampleApp");
        Directory.CreateDirectory(root);
        try
        {
            Environment.CurrentDirectory = root;
            using var writer = new StringWriter();
            var code = await GenkidamaCommandRouter.ExecuteAsync(["add", "feature", "create_order"], writer);
            var path = Path.Combine(root, "docs", "features", "CreateOrder.md");
            Assert.AreEqual(0, code);
            Assert.IsTrue(File.Exists(path));
        }
        finally
        {
            Environment.CurrentDirectory = original;
        }
    }
}
