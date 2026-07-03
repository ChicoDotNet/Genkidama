using Genkidama.Jobs;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Jobs.Tests;

/// <summary>
/// Tests the in-memory background job queue.
/// </summary>
[TestClass]
public sealed class InMemoryGenkidamaBackgroundJobQueueTests
{
    /// <summary>
    /// Verifies that enqueue and dequeue preserve job identity.
    /// </summary>
    [TestMethod]
    public void Dequeue_AfterEnqueue_ReturnsRunningJob()
    {
        var queue = new InMemoryGenkidamaBackgroundJobQueue();
        var job = StandardJob.Queued("demo");
        queue.Enqueue(job);
        var result = queue.Dequeue();
        Assert.IsTrue(result.Succeeded);
        Assert.AreEqual(job.Id, result.Value!.Id);
        Assert.AreEqual(StandardJobStatus.Running, result.Value.Status);
    }

    /// <summary>
    /// Verifies that empty dequeue returns a failure.
    /// </summary>
    [TestMethod]
    public void Dequeue_WhenEmpty_ReturnsFailure()
    {
        var queue = new InMemoryGenkidamaBackgroundJobQueue();
        var result = queue.Dequeue();
        Assert.IsFalse(result.Succeeded);
        Assert.IsNotNull(result.Problem);
    }
}
