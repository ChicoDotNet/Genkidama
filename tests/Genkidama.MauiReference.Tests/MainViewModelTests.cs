using System.ComponentModel;
using System.Net;
using Genkidama.MauiReference;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.MauiReference.Tests;

/// <summary>
/// Tests the main MAUI reference view model.
/// </summary>
[TestClass]
public sealed class MainViewModelTests
{
    /// <summary>
    /// Verifies that refresh updates the view model state.
    /// </summary>
    [TestMethod]
    public async Task RefreshAsync_WithSummary_UpdatesProperties()
    {
        const string json = "{\"succeeded\":true,\"value\":{\"title\":\"demo\",\"totalItems\":9}}";
        using var httpClient = new HttpClient(new JsonHandler(json)) { BaseAddress = Localhost() };
        var viewModel = new MainViewModel(new MauiBackendClient(httpClient));
        var changed = new List<string?>();
        viewModel.PropertyChanged += (_, args) => changed.Add(args.PropertyName);
        await viewModel.RefreshAsync();
        Assert.AreEqual("demo", viewModel.Title);
        Assert.AreEqual(9, viewModel.TotalItems);
        Assert.AreEqual("Loaded.", viewModel.StatusText);
        CollectionAssert.Contains(changed, nameof(MainViewModel.Title));
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
