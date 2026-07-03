using Genkidama.Contracts;

namespace Genkidama.Jobs;

/// <summary>
/// Stores background jobs until a worker can process them.
/// </summary>
public interface IGenkidamaBackgroundJobQueue
{
    /// <summary>
    /// Enqueues a background job.
    /// </summary>
    /// <param name="job">The job to enqueue.</param>
    /// <returns>The operation result.</returns>
    StandardResult Enqueue(StandardJob job);

    /// <summary>
    /// Attempts to dequeue the next background job.
    /// </summary>
    /// <returns>The dequeued job result.</returns>
    StandardResult<StandardJob> Dequeue();

    /// <summary>
    /// Gets the number of queued jobs.
    /// </summary>
    int Count { get; }
}
