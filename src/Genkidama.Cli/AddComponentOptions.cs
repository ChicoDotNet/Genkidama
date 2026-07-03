namespace Genkidama.Cli;

/// <summary>
/// Provides options for component generation.
/// </summary>
/// <param name="AppName">The generated application name.</param>
/// <param name="ComponentName">The component name.</param>
/// <param name="OutputDirectory">The output directory.</param>
internal sealed record AddComponentOptions(
    string AppName,
    string ComponentName,
    string OutputDirectory);
