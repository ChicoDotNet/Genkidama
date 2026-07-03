using Genkidama.Contracts;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Contracts.Tests;

/// <summary>
/// Tests the normalized standard query contract.
/// </summary>
[TestClass]
public sealed class StandardQueryTests
{
    /// <summary>
    /// Verifies that skip is calculated from one-based page values.
    /// </summary>
    [TestMethod]
    public void Skip_WithThirdPage_ReturnsExpectedOffset()
    {
        var query = new StandardQuery(3, 25);
        Assert.AreEqual(50, query.Skip);
    }
}
