using Genkidama.Jobs;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Jobs.Tests;

/// <summary>
/// Tests the standard job contract.
/// </summary>
[TestClass]
public sealed class StandardJobTests
{
    /// <summary>
    /// Verifies that queued jobs start with queued status.
    /// </summary>
    [TestMethod]
    public void Queued_WithName_ReturnsQueuedJob()
    {
        var job = StandardJob.Queued("demo");
        Assert.AreEqual("demo", job.Name);
        Assert.AreEqual(StandardJobStatus.Queued, job.Status);
        Assert.AreNotEqual(Guid.Empty, job.Id);
    }

    /// <summary>
    /// Verifies that status transitions preserve identity.
    /// </summary>
    [TestMethod]
    public void WithStatus_WithRunning_PreservesIdentity()
    {
        var job = StandardJob.Queued("demo");
        var running = job.WithStatus(StandardJobStatus.Running);
        Assert.AreEqual(job.Id, running.Id);
        Assert.AreEqual(StandardJobStatus.Running, running.Status);
    }
}
