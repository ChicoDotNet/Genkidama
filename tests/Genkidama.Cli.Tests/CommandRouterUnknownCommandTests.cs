using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests fallback routing behavior.
/// </summary>
[TestClass]
public sealed class CommandRouterUnknownCommandTests
{
    /// <summary>
    /// Verifies that unknown commands return help text without failing.
    /// </summary>
    [TestMethod]
    public async Task ExecuteAsync_WithUnknownCommand_WritesHelpText()
    {
        using var writer = new StringWriter();
        var code = await GenkidamaCommandRouter.ExecuteAsync(["unknown"], writer);
        Assert.AreEqual(0, code);
        StringAssert.Contains(writer.ToString(), "Genkidama CLI");
    }
}
