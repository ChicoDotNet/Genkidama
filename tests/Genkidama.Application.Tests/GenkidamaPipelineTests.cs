using Genkidama.Application;
using Genkidama.Contracts;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Application.Tests;

/// <summary>
/// Tests the Genkidama application pipeline.
/// </summary>
[TestClass]
public sealed class GenkidamaPipelineTests
{
    /// <summary>
    /// Verifies that behaviors wrap the terminal handler in order.
    /// </summary>
    [TestMethod]
    public async Task ExecuteAsync_WithBehaviors_RunsInOrder()
    {
        var trace = new List<string>();
        var behaviors = new[]
        {
            new RecordingBehavior("outer", trace),
            new RecordingBehavior("inner", trace)
        };

        var pipeline = new GenkidamaPipeline<TestCommand, string>(behaviors);
        var result = await pipeline.ExecuteAsync(new(), Handler);
        Assert.IsTrue(result.Succeeded);
        CollectionAssert.AreEqual(ExpectedTrace(), trace);
    }

    private static Task<StandardResult<string>> Handler(
        TestCommand command,
        CancellationToken cancellationToken)
        => Task.FromResult(StandardResult<string>.Success("done"));

    private static List<string> ExpectedTrace()
        => ["outer-before", "inner-before", "inner-after", "outer-after"];

    private sealed record TestCommand : IGenkidamaCommand<string>;

    private sealed class RecordingBehavior(
        string name,
        List<string> trace) : IGenkidamaPipelineBehavior<TestCommand, string>
    {
        public async Task<StandardResult<string>> HandleAsync(
            TestCommand request,
            GenkidamaPipelineDelegate<string> next,
            CancellationToken cancellationToken = default)
        {
            trace.Add($"{name}-before");
            var result = await next(cancellationToken);
            trace.Add($"{name}-after");
            return result;
        }
    }
}
