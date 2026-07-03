using System.Net;
using Genkidama.MauiReference;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.MauiReference.Tests;

/// <summary>
/// Tests the MAUI backend client.
/// </summary>
[TestClass]
public sealed class MauiBackendClientTests
{
    /// <summary>
    /// Verifies that a summary response is read as a typed value.
    /// </summary>
    [TestMethod]
    public async Task GetSummaryAsync_WithSuccess_ReturnsValue()
    {
        const string json = "{\"succeeded\":true,\"value\":{\"title\":\"demo\",\"totalItems\":5}}";
        using var httpClient = new HttpClient(new JsonHandler(json)) { BaseAddress = Localhost() };
        var result = await new MauiBackendClient(httpClient).GetSummaryAsync();
        Assert.IsTrue(result.Succeeded);
        Assert.AreEqual("demo", result.Value!.Title);
        Assert.AreEqual(5, result.Value.TotalItems);
    }

    private static Uri Localhost()
        => new("http://localhost/");

    private sealed class JsonHandler(string json) : HttpMessageHandler
    {
        protected override Task<HttpResponseMessage> SendAsync(
            HttpRequestMessage request,
            CancellationToken cancellationToken)
            => Task.FromResult(new HttpResponseMessage(HttpStatusCode.OK) { Content = new StringContent(json) });
    }
}
