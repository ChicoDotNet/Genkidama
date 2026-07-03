namespace Genkidama.Cli;

/// <summary>
/// Writes reusable block files.
/// </summary>
internal static class AddBlockRunner
{
    /// <summary>
    /// Runs block file generation.
    /// </summary>
    internal static Task<int> RunAsync(AddComponentOptions options, TextWriter writer)
    {
        foreach (var file in BlockTemplate.Create(options.AppName, options.ComponentName))
        {
            file.WriteTo(options.OutputDirectory);
        }

        writer.WriteLine($"Added block {BlockName.Normalize(options.ComponentName)}.");
        return Task.FromResult(0);
    }
}
