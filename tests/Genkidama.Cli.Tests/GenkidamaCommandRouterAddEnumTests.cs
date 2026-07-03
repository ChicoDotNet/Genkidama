using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests add enum routing.
/// </summary>
[TestClass]
public sealed class GenkidamaCommandRouterAddEnumTests
{
    /// <summary>
    /// Verifies that add enum uses the current directory name as the app name.
    /// </summary>
    [TestMethod]
    public async Task ExecuteAsync_WithAddEnum_WritesEnumInCurrentApp()
    {
        var original = Environment.CurrentDirectory;
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"), "SampleApp");
        Directory.CreateDirectory(root);
        try
        {
            Environment.CurrentDirectory = root;
            using var writer = new StringWriter();
            var code = await GenkidamaCommandRouter.ExecuteAsync(["add", "enum", "order_status", "draft"], writer);
            var path = Path.Combine(root, "src", "SampleApp.Domain", "Enums", "OrderStatus.cs");
            Assert.AreEqual(0, code);
            Assert.IsTrue(File.Exists(path));
        }
        finally
        {
            Environment.CurrentDirectory = original;
        }
    }
}
