using Genkidama.Data;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Data.Tests;

/// <summary>
/// Tests supported database providers.
/// </summary>
[TestClass]
public sealed class GenkidamaDatabaseProviderTests
{
    /// <summary>
    /// Verifies that the core supports only the approved providers.
    /// </summary>
    [TestMethod]
    public void Values_ReturnsApprovedProviders()
    {
        var providers = Enum.GetNames<GenkidamaDatabaseProvider>();
        CollectionAssert.AreEquivalent(
            new[] { "MariaDb", "Sqlite", "SqlServer", "PostgreSql" },
            providers);
    }
}
