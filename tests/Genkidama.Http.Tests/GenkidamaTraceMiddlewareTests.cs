using Genkidama.Http;
using Microsoft.AspNetCore.Http;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Http.Tests;

/// <summary>
/// Tests the Genkidama trace middleware.
/// </summary>
[TestClass]
public sealed class GenkidamaTraceMiddlewareTests
{
    /// <summary>
    /// Verifies that the trace header is written before the next delegate completes.
    /// </summary>
    [TestMethod]
    public async Task InvokeAsync_WithContext_AddsTraceHeader()
    {
        var context = new DefaultHttpContext { TraceIdentifier = "trace-002" };
        var middleware = new GenkidamaTraceMiddleware(_ => Task.CompletedTask);
        await middleware.InvokeAsync(context);
        Assert.AreEqual("trace-002", context.Response.Headers[GenkidamaHttpNames.TraceIdHeader].ToString());
    }
}
