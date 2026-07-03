using System.Net;
using Genkidama.ConsoleReference;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.ConsoleReference.Tests;

/// <summary>
/// Tests the typed console backend client.
/// </summary>
[TestClass]
public sealed class ConsoleBackendClientTests
{
    /// <summary>
    /// Verifies that successful standard results are read as typed values.
    /// </summary>
    [TestMethod]
    public async Task GetResultAsync_WithSuccessResponse_ReturnsTypedValue()
    {
        const string json = "{\"succeeded\":true,\"value\":{\"name\":\"demo\"}}";
        using var httpClient = new HttpClient(new JsonHandler(json)) { BaseAddress = Localhost() };
        var result = await new ConsoleBackendClient(httpClient).GetResultAsync<DemoValue>("demo");
        Assert.IsTrue(result.Succeeded);
        Assert.AreEqual("demo", result.Value!.Name);
    }

    /// <summary>
    /// Verifies that HTTP failures are mapped to a typed API result.
    /// </summary>
    [TestMethod]
    public async Task GetResultAsync_WithHttpFailure_ReturnsFailureResult()
    {
        using var httpClient = new HttpClient(new JsonHandler("{}", HttpStatusCode.BadRequest)) { BaseAddress = Localhost() };
        var result = await new ConsoleBackendClient(httpClient).GetResultAsync<DemoValue>("demo");
        Assert.IsFalse(result.Succeeded);
        Assert.IsNotNull(result.Problem);
    }

    private static Uri Localhost()
        => new("http://localhost/");

    private sealed record DemoValue(string Name);

    private sealed class JsonHandler(string json, HttpStatusCode statusCode = HttpStatusCode.OK) : HttpMessageHandler
    {
        protected override Task<HttpResponseMessage> SendAsync(
            HttpRequestMessage request,
            CancellationToken cancellationToken)
            => Task.FromResult(new HttpResponseMessage(statusCode) { Content = new StringContent(json) });
    }
}
