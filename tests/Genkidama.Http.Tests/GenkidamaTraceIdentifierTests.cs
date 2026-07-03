using Genkidama.Http;
using Microsoft.AspNetCore.Http;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Http.Tests;

/// <summary>
/// Tests trace identifier helpers.
/// </summary>
[TestClass]
public sealed class GenkidamaTraceIdentifierTests
{
    /// <summary>
    /// Verifies that the HTTP context trace identifier is reused.
    /// </summary>
    [TestMethod]
    public void Get_WithContextTraceIdentifier_ReturnsExistingValue()
    {
        var context = new DefaultHttpContext { TraceIdentifier = "trace-001" };
        var traceId = GenkidamaTraceIdentifier.Get(context);
        Assert.AreEqual("trace-001", traceId);
    }
}
