namespace Genkidama.Cli;

/// <summary>
/// Generates an entity file for a Genkidama application.
/// </summary>
internal static class GenkidamaAddEntityCommand
{
    /// <summary>
    /// Executes the entity generation command.
    /// </summary>
    /// <param name="options">The generation options.</param>
    /// <param name="writer">The output writer.</param>
    /// <returns>The process exit code.</returns>
    internal static Task<int> ExecuteAsync(AddEntityOptions options, TextWriter writer)
    {
        var file = EntityTemplate.Create(options.AppName, options.EntityName);
        file.WriteTo(options.OutputDirectory);
        writer.WriteLine($"Added entity {EntityName.Normalize(options.EntityName)}.");
        return Task.FromResult(0);
    }
}
