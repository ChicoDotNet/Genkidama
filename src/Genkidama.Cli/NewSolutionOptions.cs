namespace Genkidama.Cli;

/// <summary>
/// Provides options for generating a new Genkidama solution.
/// </summary>
internal sealed record NewSolutionOptions(string AppName, string OutputDirectory);
