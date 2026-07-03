using Genkidama.Application;
using Genkidama.Contracts;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Application.Tests;

/// <summary>
/// Tests the query dispatcher.
/// </summary>
[TestClass]
public sealed class GenkidamaQueryDispatcherTests
{
    /// <summary>
    /// Verifies that a query is sent to its handler.
    /// </summary>
    [TestMethod]
    public async Task SendAsync_WithHandler_ReturnsHandlerResult()
    {
        var dispatcher = new GenkidamaQueryDispatcher();
        var result = await dispatcher.SendAsync(new GetThing(7), new Handler(), []);
        Assert.IsTrue(result.Succeeded);
        Assert.AreEqual("thing:7", result.Value);
    }

    private sealed record GetThing(int Id) : IGenkidamaQuery<string>;

    private sealed class Handler : IGenkidamaQueryHandler<GetThing, string>
    {
        public Task<StandardResult<string>> HandleAsync(
            GetThing query,
            CancellationToken cancellationToken = default)
            => Task.FromResult(StandardResult<string>.Success($"thing:{query.Id}"));
    }
}
