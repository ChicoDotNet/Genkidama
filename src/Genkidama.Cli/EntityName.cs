namespace Genkidama.Cli;

/// <summary>
/// Normalizes entity names for generated source files.
/// </summary>
internal static class EntityName
{
    /// <summary>
    /// Converts a raw entity name to PascalCase.
    /// </summary>
    /// <param name="value">The raw entity name.</param>
    /// <returns>The normalized entity name.</returns>
    internal static string Normalize(string value)
        => string.Concat(Parts(value).Select(Capitalize));

    private static IEnumerable<string> Parts(string value)
        => value.Split(['-', '_', ' '], StringSplitOptions.RemoveEmptyEntries);

    private static string Capitalize(string value)
        => string.IsNullOrWhiteSpace(value)
            ? string.Empty
            : char.ToUpperInvariant(value[0]) + value[1..];
}
