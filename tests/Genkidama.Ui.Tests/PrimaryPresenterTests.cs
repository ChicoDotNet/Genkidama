using System.Net;
using Genkidama.DesktopReference;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Ui.Tests;

/// <summary>
/// Tests the primary presenter.
/// </summary>
[TestClass]
public sealed class PrimaryPresenterTests
{
    /// <summary>
    /// Verifies that the presenter shows summary data.
    /// </summary>
    [TestMethod]
    public async Task LoadAsync_WithSummary_ShowsSummary()
    {
        const string json = "{\"succeeded\":true,\"value\":{\"title\":\"demo\",\"totalItems\":3}}";
        using var httpClient = new HttpClient(new JsonHandler(json)) { BaseAddress = Localhost() };
        var view = new FakeView();
        await new PrimaryPresenter(view, new DesktopBackendClient(httpClient)).LoadAsync();
        Assert.AreEqual("demo", view.Title);
        Assert.AreEqual(3, view.TotalItems);
    }

    private static Uri Localhost()
        => new("http://localhost/");

    private sealed class FakeView : IPrimaryView
    {
        public string? Title { get; private set; }
        public int TotalItems { get; private set; }

        public void ShowLoading()
        {
        }

        public void ShowSummary(SummaryModel summary)
        {
            Title = summary.Title;
            TotalItems = summary.TotalItems;
        }

        public void ShowText(string text)
            => Title = text;
    }

    private sealed class JsonHandler(string json) : HttpMessageHandler
    {
        protected override Task<HttpResponseMessage> SendAsync(
            HttpRequestMessage request,
            CancellationToken cancellationToken)
            => Task.FromResult(new HttpResponseMessage(HttpStatusCode.OK) { Content = new StringContent(json) });
    }
}
