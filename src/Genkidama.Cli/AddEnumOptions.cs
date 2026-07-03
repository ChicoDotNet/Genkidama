namespace Genkidama.Cli;

/// <summary>
/// Provides options for enum generation.
/// </summary>
/// <param name="AppName">The generated application name.</param>
/// <param name="EnumName">The enum name.</param>
/// <param name="Values">The enum values.</param>
/// <param name="OutputDirectory">The output directory.</param>
internal sealed record AddEnumOptions(
    string AppName,
    string EnumName,
    IReadOnlyList<string> Values,
    string OutputDirectory);
