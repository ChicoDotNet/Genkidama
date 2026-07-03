namespace Genkidama.ConsoleReference;

/// <summary>
/// Provides backend configuration for the console reference client.
/// </summary>
/// <param name="BaseAddress">The backend base address.</param>
public sealed record ConsoleBackendOptions(Uri BaseAddress)
{
    /// <summary>
    /// Creates options from a URL string.
    /// </summary>
    /// <param name="baseAddress">The backend base address.</param>
    /// <returns>The backend options.</returns>
    public static ConsoleBackendOptions From(string baseAddress)
        => new(new Uri(baseAddress, UriKind.Absolute));
}
