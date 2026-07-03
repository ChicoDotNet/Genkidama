namespace Genkidama.Contracts;

/// <summary>
/// Represents the normalized result of a Genkidama operation with a value.
/// </summary>
/// <typeparam name="TValue">The value type.</typeparam>
public sealed record StandardResult<TValue>
{
    private StandardResult(bool succeeded, TValue? value, StandardProblem? problem)
    {
        Succeeded = succeeded;
        Value = value;
        Problem = problem;
    }

    /// <summary>
    /// Gets a value indicating whether the operation succeeded.
    /// </summary>
    public bool Succeeded { get; }

    /// <summary>
    /// Gets the operation value when the operation succeeded.
    /// </summary>
    public TValue? Value { get; }

    /// <summary>
    /// Gets the problem when the operation failed.
    /// </summary>
    public StandardProblem? Problem { get; }

    /// <summary>
    /// Creates a successful result.
    /// </summary>
    /// <param name="value">The operation value.</param>
    /// <returns>A successful result.</returns>
    public static StandardResult<TValue> Success(TValue value)
        => new(true, value, null);

    /// <summary>
    /// Creates a failed result.
    /// </summary>
    /// <param name="problem">The problem that caused the failure.</param>
    /// <returns>A failed result.</returns>
    public static StandardResult<TValue> Failure(StandardProblem problem)
        => new(false, default, problem);
}
