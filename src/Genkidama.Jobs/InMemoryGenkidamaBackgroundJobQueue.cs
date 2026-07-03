using Genkidama.Contracts;

namespace Genkidama.Jobs;

/// <summary>
/// Provides an in-memory background job queue for tests and generated samples.
/// </summary>
public sealed class InMemoryGenkidamaBackgroundJobQueue : IGenkidamaBackgroundJobQueue
{
    private readonly Queue<StandardJob> jobs = new();

    /// <inheritdoc />
    public int Count => jobs.Count;

    /// <inheritdoc />
    public StandardResult Enqueue(StandardJob job)
    {
        jobs.Enqueue(job);
        return StandardResult.Success();
    }

    /// <inheritdoc />
    public StandardResult<StandardJob> Dequeue()
        => jobs.TryDequeue(out var job)
            ? StandardResult<StandardJob>.Success(job.WithStatus(StandardJobStatus.Running))
            : StandardResult<StandardJob>.Failure(StandardProblem.Validation("No jobs are queued."));
}
