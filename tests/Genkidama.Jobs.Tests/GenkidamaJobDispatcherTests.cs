using Genkidama.Contracts;
using Genkidama.Jobs;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Jobs.Tests;

/// <summary>
/// Tests the job dispatcher.
/// </summary>
[TestClass]
public sealed class GenkidamaJobDispatcherTests
{
    /// <summary>
    /// Verifies that the dispatcher sends jobs to handlers.
    /// </summary>
    [TestMethod]
    public async Task SendAsync_WithHandler_ReturnsHandlerResult()
    {
        var dispatcher = new GenkidamaJobDispatcher();
        var result = await dispatcher.SendAsync(new DemoJob("alpha"), new Handler());
        Assert.IsTrue(result.Succeeded);
        Assert.AreEqual("handled:alpha", result.Value);
    }

    private sealed record DemoJob(string Name) : IGenkidamaJob<string>;

    private sealed class Handler : IGenkidamaJobHandler<DemoJob, string>
    {
        public Task<StandardResult<string>> HandleAsync(
            DemoJob job,
            CancellationToken cancellationToken = default)
            => Task.FromResult(StandardResult<string>.Success($"handled:{job.Name}"));
    }
}
