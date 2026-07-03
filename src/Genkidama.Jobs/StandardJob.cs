namespace Genkidama.Jobs;

/// <summary>
/// Describes a background job tracked by Genkidama.
/// </summary>
/// <param name="Id">The job identifier.</param>
/// <param name="Name">The job name.</param>
/// <param name="CreatedUtc">The UTC creation timestamp.</param>
/// <param name="Status">The current status.</param>
public sealed record StandardJob(
    Guid Id,
    string Name,
    DateTimeOffset CreatedUtc,
    StandardJobStatus Status)
{
    /// <summary>
    /// Creates a queued job using the current UTC timestamp.
    /// </summary>
    /// <param name="name">The job name.</param>
    /// <returns>The queued job.</returns>
    public static StandardJob Queued(string name)
        => new(Guid.NewGuid(), name, DateTimeOffset.UtcNow, StandardJobStatus.Queued);

    /// <summary>
    /// Creates a copy of this job with a different status.
    /// </summary>
    /// <param name="status">The new status.</param>
    /// <returns>The updated job.</returns>
    public StandardJob WithStatus(StandardJobStatus status)
        => this with { Status = status };
}
