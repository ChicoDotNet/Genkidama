using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests add entity routing.
/// </summary>
[TestClass]
public sealed class GenkidamaCommandRouterAddEntityTests
{
    /// <summary>
    /// Verifies that add entity uses the current directory name as the app name.
    /// </summary>
    [TestMethod]
    public async Task ExecuteAsync_WithAddEntity_WritesEntityInCurrentApp()
    {
        var original = Environment.CurrentDirectory;
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"), "SampleApp");
        Directory.CreateDirectory(root);
        try
        {
            Environment.CurrentDirectory = root;
            using var writer = new StringWriter();
            var code = await GenkidamaCommandRouter.ExecuteAsync(["add", "entity", "customer"], writer);
            var path = Path.Combine(root, "src", "SampleApp.Domain", "Entities", "Customer.cs");
            Assert.AreEqual(0, code);
            Assert.IsTrue(File.Exists(path));
        }
        finally
        {
            Environment.CurrentDirectory = original;
        }
    }
}
