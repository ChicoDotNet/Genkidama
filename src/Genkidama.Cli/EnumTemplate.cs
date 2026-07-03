namespace Genkidama.Cli;

/// <summary>
/// Creates generated enum files.
/// </summary>
internal static class EnumTemplate
{
    /// <summary>
    /// Creates the generated enum source file.
    /// </summary>
    /// <param name="appName">The generated application name.</param>
    /// <param name="enumName">The enum name.</param>
    /// <param name="values">The enum values.</param>
    /// <returns>The generated file.</returns>
    internal static GeneratedFile Create(
        string appName,
        string enumName,
        IReadOnlyList<string> values)
    {
        var name = EnumName.Normalize(enumName);
        return new GeneratedFile(
            $"src/{appName}.Domain/Enums/{name}.cs",
            Content(appName, name, NormalizeValues(values)));
    }

    private static IReadOnlyList<string> NormalizeValues(IReadOnlyList<string> values)
        => values.Count == 0 ? ["Unknown"] : values.Select(EnumName.Normalize).ToArray();

    private static string Content(string appName, string enumName, IReadOnlyList<string> values)
        => $"namespace {appName}.Domain.Enums;\n\n" +
           $"/// <summary>\n/// Represents the {enumName} enum.\n/// </summary>\n" +
           $"public enum {enumName}\n{{\n" + Entries(values) + "}\n";

    private static string Entries(IReadOnlyList<string> values)
        => string.Join(",\n", values.Select((value, index) => $"    {value} = {index}")) + "\n";
}
