namespace Genkidama.Cli;

/// <summary>
/// Provides options for feature generation.
/// </summary>
/// <param name="AppName">The generated application name.</param>
/// <param name="FeatureName">The feature name.</param>
/// <param name="OutputDirectory">The output directory.</param>
internal sealed record AddFeatureOptions(
    string AppName,
    string FeatureName,
    string OutputDirectory);
