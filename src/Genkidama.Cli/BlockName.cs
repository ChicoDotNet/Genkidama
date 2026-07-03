namespace Genkidama.Cli;

/// <summary>
/// Normalizes block names for generated source files.
/// </summary>
internal static class BlockName
{
    /// <summary>
    /// Converts a raw block name to PascalCase.
    /// </summary>
    /// <param name="value">The raw block name.</param>
    /// <returns>The normalized block name.</returns>
    internal static string Normalize(string value)
        => string.Concat(Parts(value).Select(Capitalize));

    private static IEnumerable<string> Parts(string value)
        => value.Split(['-', '_', ' '], StringSplitOptions.RemoveEmptyEntries);

    private static string Capitalize(string value)
        => string.IsNullOrWhiteSpace(value)
            ? string.Empty
            : char.ToUpperInvariant(value[0]) + value[1..];
}
