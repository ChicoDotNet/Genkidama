namespace Genkidama.MauiReference;

/// <summary>
/// Represents a typed result for MAUI clients.
/// </summary>
/// <typeparam name="TValue">The value type.</typeparam>
public sealed class MauiClientResult<TValue>
{
    /// <summary>Gets or initializes whether the operation succeeded.</summary>
    public bool Succeeded { get; init; }

    /// <summary>Gets or initializes the optional value.</summary>
    public TValue? Value { get; init; }

    /// <summary>Gets or initializes optional text.</summary>
    public string? Text { get; init; }
}
