namespace Genkidama.Cli;

/// <summary>
/// Provides options for entity generation.
/// </summary>
/// <param name="AppName">The generated application name.</param>
/// <param name="EntityName">The entity name.</param>
/// <param name="OutputDirectory">The output directory.</param>
internal sealed record AddEntityOptions(
    string AppName,
    string EntityName,
    string OutputDirectory);
