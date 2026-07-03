using Genkidama.Contracts;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Contracts.Tests;

/// <summary>
/// Tests the normalized collection result contract.
/// </summary>
[TestClass]
public sealed class StandardCollectionResultTests
{
    /// <summary>
    /// Verifies that a collection result preserves items and total count.
    /// </summary>
    [TestMethod]
    public void From_WithItems_PreservesValues()
    {
        var query = StandardQuery.FirstPage();
        var result = StandardCollectionResult<string>.From(["one", "two"], 10, query);
        Assert.AreEqual(2, result.Items.Count);
        Assert.AreEqual(10, result.TotalCount);
        Assert.AreEqual(query, result.Query);
    }
}
