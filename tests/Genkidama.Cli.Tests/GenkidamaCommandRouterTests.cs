using Genkidama.Cli;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests the bootstrap command router.
/// </summary>
[TestClass]
public sealed class GenkidamaCommandRouterTests
{
    /// <summary>
    /// Verifies that the bootstrap command returns the CLI identifier.
    /// </summary>
    /// <returns>A task representing the asynchronous test.</returns>
    [TestMethod]
    public async Task ExecuteAsync_WithVersionArgument_WritesIdentifier()
    {
        using var writer = new StringWriter();
        var code = await GenkidamaCommandRouter.ExecuteAsync(["--version"], writer);
        Assert.AreEqual(0, code);
        Assert.AreEqual("0.1.0-gen001", writer.ToString().Trim());
    }
}
