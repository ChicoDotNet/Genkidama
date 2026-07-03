namespace Genkidama.Blueprints;

/// <summary>
/// Describes an output file.
/// </summary>
/// <param name="RelativePath">The relative path.</param>
/// <param name="Content">The content.</param>
public sealed record BlueprintFile(string RelativePath, string Content);
