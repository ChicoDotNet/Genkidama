namespace Genkidama.ConsoleReference;

/// <summary>
/// Represents a typed API result for console clients.
/// </summary>
/// <typeparam name="TValue">The value type.</typeparam>
public sealed class ApiResult<TValue>
{
    /// <summary>
    /// Gets or initializes whether the operation succeeded.
    /// </summary>
    public bool Succeeded { get; init; }

    /// <summary>
    /// Gets or initializes the optional value.
    /// </summary>
    public TValue? Value { get; init; }

    /// <summary>
    /// Gets or initializes the optional issue.
    /// </summary>
    public StandardApiProblem? Problem { get; init; }
}
