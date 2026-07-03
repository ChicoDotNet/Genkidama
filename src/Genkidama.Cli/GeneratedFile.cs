namespace Genkidama.Cli;

/// <summary>
/// Describes a generated file before it is written to disk.
/// </summary>
internal sealed record GeneratedFile(string RelativePath, string Content)
{
    /// <summary>
    /// Writes the generated file into the selected root directory.
    /// </summary>
    /// <param name="rootDirectory">The generation root directory.</param>
    internal void WriteTo(string rootDirectory)
    {
        var path = Path.Combine(rootDirectory, RelativePath);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, Content);
    }
}
