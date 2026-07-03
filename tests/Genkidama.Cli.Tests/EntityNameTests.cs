using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests entity name normalization.
/// </summary>
[TestClass]
public sealed class EntityNameTests
{
    /// <summary>
    /// Verifies that common separators are converted to PascalCase.
    /// </summary>
    [TestMethod]
    public void Normalize_WithSeparatedWords_ReturnsPascalCase()
    {
        var name = EntityName.Normalize("sales_order");
        Assert.AreEqual("SalesOrder", name);
    }
}
