namespace Genkidama.Cli;

/// <summary>
/// Generates a new Genkidama solution.
/// </summary>
internal static class GenkidamaNewCommand
{
    /// <summary>
    /// Executes the solution generation command.
    /// </summary>
    /// <param name="options">The generation options.</param>
    /// <param name="writer">The output writer.</param>
    /// <returns>The process exit code.</returns>
    internal static Task<int> ExecuteAsync(NewSolutionOptions options, TextWriter writer)
    {
        var root = Path.Combine(options.OutputDirectory, options.AppName);
        foreach (var file in NewSolutionTemplate.Create(options.AppName))
        {
            file.WriteTo(root);
        }

        writer.WriteLine($"Created {options.AppName}.");
        return Task.FromResult(0);
    }
}
