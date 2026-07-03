namespace Genkidama.Contracts;

/// <summary>
/// Represents the normalized result of a Genkidama operation.
/// </summary>
public sealed record StandardResult
{
    private StandardResult(bool succeeded, StandardProblem? problem)
    {
        Succeeded = succeeded;
        Problem = problem;
    }

    /// <summary>
    /// Gets a value indicating whether the operation succeeded.
    /// </summary>
    public bool Succeeded { get; }

    /// <summary>
    /// Gets the problem when the operation failed.
    /// </summary>
    public StandardProblem? Problem { get; }

    /// <summary>
    /// Creates a successful result.
    /// </summary>
    /// <returns>A successful result.</returns>
    public static StandardResult Success()
        => new(true, null);

    /// <summary>
    /// Creates a failed result.
    /// </summary>
    /// <param name="problem">The problem that caused the failure.</param>
    /// <returns>A failed result.</returns>
    public static StandardResult Failure(StandardProblem problem)
        => new(false, problem);
}
