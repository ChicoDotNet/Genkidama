using Genkidama.Http;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Http.Tests;

/// <summary>
/// Tests the HTTP query attribute.
/// </summary>
[TestClass]
public sealed class HttpQueryAttributeTests
{
    /// <summary>
    /// Verifies that the optional prefix is preserved.
    /// </summary>
    [TestMethod]
    public void Constructor_WithPrefix_PreservesPrefix()
    {
        var attribute = new HttpQueryAttribute("filter");
        Assert.AreEqual("filter", attribute.Prefix);
    }
}
