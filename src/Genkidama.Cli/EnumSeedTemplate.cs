namespace Genkidama.Cli;

/// <summary>
/// Creates generated enum seed files.
/// </summary>
internal static class EnumSeedTemplate
{
    /// <summary>
    /// Creates the generated enum seed file.
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
            $"src/{appName}.Persistence/Seeds/Enums/{name}.json",
            Content(name, NormalizeValues(values)));
    }

    private static IReadOnlyList<string> NormalizeValues(IReadOnlyList<string> values)
        => values.Count == 0 ? ["Unknown"] : values.Select(EnumName.Normalize).ToArray();

    private static string Content(string enumName, IReadOnlyList<string> values)
        => "{\n" +
           $"  \"schema\": \"enum\",\n" +
           $"  \"name\": \"{enumName}\",\n" +
           "  \"values\": [\n" + SeedValues(values) +
           "  ]\n" +
           "}\n";

    private static string SeedValues(IReadOnlyList<string> values)
        => string.Join(",\n", values.Select((value, index) => $"    {{ \"name\": \"{value}\", \"value\": {index} }}")) + "\n";
}
