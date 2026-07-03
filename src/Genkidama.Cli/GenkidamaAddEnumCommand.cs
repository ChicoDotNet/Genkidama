namespace Genkidama.Cli;

/// <summary>
/// Generates enum files for a Genkidama application.
/// </summary>
internal static class GenkidamaAddEnumCommand
{
    /// <summary>
    /// Executes the enum generation command.
    /// </summary>
    /// <param name="options">The generation options.</param>
    /// <param name="writer">The output writer.</param>
    /// <returns>The process exit code.</returns>
    internal static Task<int> ExecuteAsync(AddEnumOptions options, TextWriter writer)
    {
        foreach (var file in Files(options))
        {
            file.WriteTo(options.OutputDirectory);
        }

        writer.WriteLine($"Added enum {EnumName.Normalize(options.EnumName)}.");
        return Task.FromResult(0);
    }

    private static IReadOnlyList<GeneratedFile> Files(AddEnumOptions options)
        =>
        [
            EnumTemplate.Create(options.AppName, options.EnumName, options.Values),
            EnumSeedTemplate.Create(options.AppName, options.EnumName, options.Values)
        ];
}
