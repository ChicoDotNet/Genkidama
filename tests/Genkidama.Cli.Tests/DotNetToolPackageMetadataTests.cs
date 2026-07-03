using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests packaged CLI behavior.
/// </summary>
[TestClass]
public sealed class DotNetToolPackageMetadataTests
{
    /// <summary>
    /// Verifies that the CLI version matches the packaged version.
    /// </summary>
    [TestMethod]
    public async Task ExecuteAsync_WithVersion_WritesPackagedVersion()
    {
        using var writer = new StringWriter();
        var code = await GenkidamaCommandRouter.ExecuteAsync(["--version"], writer);
        Assert.AreEqual(0, code);
        Assert.AreEqual("0.1.0-alpha.22", writer.ToString().Trim());
    }
}
