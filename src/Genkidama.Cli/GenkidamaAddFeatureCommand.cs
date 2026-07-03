namespace Genkidama.Cli;

/// <summary>
/// Generates a vertical slice feature for a Genkidama application.
/// </summary>
internal static class GenkidamaAddFeatureCommand
{
    /// <summary>
    /// Executes the feature generation command.
    /// </summary>
    /// <param name="options">The generation options.</param>
    /// <param name="writer">The output writer.</param>
    /// <returns>The process exit code.</returns>
    internal static Task<int> ExecuteAsync(AddFeatureOptions options, TextWriter writer)
    {
        foreach (var file in FeatureTemplate.Create(options.AppName, options.FeatureName))
        {
            file.WriteTo(options.OutputDirectory);
        }

        writer.WriteLine($"Added feature {FeatureName.Normalize(options.FeatureName)}.");
        return Task.FromResult(0);
    }
}
