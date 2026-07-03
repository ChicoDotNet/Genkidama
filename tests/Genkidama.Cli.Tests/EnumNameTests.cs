using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests enum name normalization.
/// </summary>
[TestClass]
public sealed class EnumNameTests
{
    /// <summary>
    /// Verifies that common separators are converted to PascalCase.
    /// </summary>
    [TestMethod]
    public void Normalize_WithSeparatedWords_ReturnsPascalCase()
    {
        var name = EnumName.Normalize("order_status");
        Assert.AreEqual("OrderStatus", name);
    }
}
