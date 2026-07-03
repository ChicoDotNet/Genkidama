using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests block name normalization.
/// </summary>
[TestClass]
public sealed class BlockNameTests
{
    /// <summary>
    /// Verifies that common separators are converted to PascalCase.
    /// </summary>
    [TestMethod]
    public void Normalize_WithSeparatedWords_ReturnsPascalCase()
    {
        var name = BlockName.Normalize("audit_log");
        Assert.AreEqual("AuditLog", name);
    }
}
