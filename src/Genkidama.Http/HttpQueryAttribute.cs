namespace Genkidama.Http;

/// <summary>
/// Marks a request type as bindable from HTTP query string values.
/// </summary>
[AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Parameter)]
public sealed class HttpQueryAttribute : Attribute
{
    /// <summary>
    /// Initializes a new instance of the <see cref="HttpQueryAttribute"/> class.
    /// </summary>
    public HttpQueryAttribute()
    {
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="HttpQueryAttribute"/> class.
    /// </summary>
    /// <param name="prefix">The optional query string prefix.</param>
    public HttpQueryAttribute(string prefix)
        => Prefix = prefix;

    /// <summary>
    /// Gets the optional query string prefix.
    /// </summary>
    public string? Prefix { get; }
}
