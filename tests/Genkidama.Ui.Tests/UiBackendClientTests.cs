using System.Net;
using Genkidama.DesktopReference;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Ui.Tests;

/// <summary>
/// Tests the desktop backend client.
/// </summary>
[TestClass]
public sealed class UiBackendClientTests
{
    /// <summary>
    /// Verifies that the summary endpoint returns a typed value.
    /// </summary>
    [TestMethod]
    public async Task GetSummaryAsync_WithSuccess_ReturnsValue()
    {
        const string json = "{\"succeeded\":true,\"value\":{\"title\":\"demo\",\"totalItems\":7}}";
        using var httpClient = new HttpClient(new JsonHandler(json)) { BaseAddress = Localhost() };
        var result = await new DesktopBackendClient(httpClient).GetSummaryAsync();
        Assert.IsTrue(result.Succeeded);
        Assert.AreEqual("demo", result.Value!.Title);
        Assert.AreEqual(7, result.Value.TotalItems);
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
