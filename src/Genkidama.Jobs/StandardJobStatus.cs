namespace Genkidama.Jobs;

/// <summary>
/// Identifies the lifecycle status of a Genkidama job.
/// </summary>
public enum StandardJobStatus
{
    /// <summary>The job has been queued.</summary>
    Queued,

    /// <summary>The job is running.</summary>
    Running,

    /// <summary>The job completed successfully.</summary>
    Succeeded,

    /// <summary>The job failed.</summary>
    Failed,

    /// <summary>The job was cancelled.</summary>
    Cancelled
}
