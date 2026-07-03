using Genkidama.ConsoleReference;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.ConsoleReference.Tests;

/// <summary>
/// Tests console backend options.
/// </summary>
[TestClass]
public sealed class ConsoleBackendOptionsTests
{
    /// <summary>
    /// Verifies that options can be created from a URL string.
    /// </summary>
    [TestMethod]
    public void From_WithAbsoluteUrl_ReturnsOptions()
    {
        var options = ConsoleBackendOptions.From("http://localhost:5000/");
        Assert.AreEqual("http://localhost:5000/", options.BaseAddress.ToString());
    }
}
