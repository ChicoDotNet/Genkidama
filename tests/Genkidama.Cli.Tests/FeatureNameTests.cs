using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests feature name normalization.
/// </summary>
[TestClass]
public sealed class FeatureNameTests
{
    /// <summary>
    /// Verifies that common separators are converted to PascalCase.
    /// </summary>
    [TestMethod]
    public void Normalize_WithSeparatedWords_ReturnsPascalCase()
    {
        var name = FeatureName.Normalize("create_order");
        Assert.AreEqual("CreateOrder", name);
    }
}
