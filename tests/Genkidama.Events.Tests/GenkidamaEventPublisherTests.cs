using Genkidama.Contracts;
using Genkidama.Events;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Events.Tests;

/// <summary>
/// Tests the event publisher.
/// </summary>
[TestClass]
public sealed class GenkidamaEventPublisherTests
{
    /// <summary>
    /// Verifies that all successful handlers are executed.
    /// </summary>
    [TestMethod]
    public async Task PublishAsync_WithSuccessfulHandlers_RunsAllHandlers()
    {
        var trace = new List<string>();
        var handlers = new[] { new RecordingHandler("a", trace), new RecordingHandler("b", trace) };
        var result = await new GenkidamaEventPublisher().PublishAsync(StandardEvent.Create("demo"), handlers);
        Assert.IsTrue(result.Succeeded);
        CollectionAssert.AreEqual(new[] { "a", "b" }, trace);
    }

    /// <summary>
    /// Verifies that the first failed handler result is preserved.
    /// </summary>
    [TestMethod]
    public async Task PublishAsync_WithFailedHandler_ReturnsFailure()
    {
        var handlers = new IGenkidamaEventHandler<StandardEvent>[] { new FailedHandler() };
        var result = await new GenkidamaEventPublisher().PublishAsync(StandardEvent.Create("demo"), handlers);
        Assert.IsFalse(result.Succeeded);
        Assert.IsNotNull(result.Problem);
    }

    private sealed class RecordingHandler(string name, List<string> trace)
        : IGenkidamaEventHandler<StandardEvent>
    {
        public Task<StandardResult> HandleAsync(
            StandardEvent eventItem,
            CancellationToken cancellationToken = default)
        {
            trace.Add(name);
            return Task.FromResult(StandardResult.Success());
        }
    }

    private sealed class FailedHandler : IGenkidamaEventHandler<StandardEvent>
    {
        public Task<StandardResult> HandleAsync(
            StandardEvent eventItem,
            CancellationToken cancellationToken = default)
            => Task.FromResult(StandardResult.Failure(StandardProblem.Validation("failed")));
    }
}
