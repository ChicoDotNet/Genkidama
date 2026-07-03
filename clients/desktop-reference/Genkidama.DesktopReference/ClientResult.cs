namespace Genkidama.DesktopReference;

/// <summary>
/// Represents a typed result for desktop clients.
/// </summary>
/// <typeparam name="TValue">The value type.</typeparam>
public sealed class ClientResult<TValue>
{
    /// <summary>Gets or initializes whether the operation succeeded.</summary>
    public bool Succeeded { get; init; }

    /// <summary>Gets or initializes the optional value.</summary>
    public TValue? Value { get; init; }

    /// <summary>Gets or initializes the optional issue.</summary>
    public ClientIssue? Issue { get; init; }
}
