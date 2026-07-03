namespace Genkidama.Cli;

/// <summary>
/// Creates the first generated Genkidama solution skeleton.
/// </summary>
internal static class NewSolutionTemplate
{
    /// <summary>
    /// Creates generated files for the selected application name.
    /// </summary>
    /// <param name="appName">The generated application name.</param>
    /// <returns>The generated files.</returns>
    internal static IReadOnlyList<GeneratedFile> Create(string appName)
        => [Readme(appName), .. ProjectFiles(appName)];

    private static GeneratedFile Readme(string appName)
        => new("README.md", $"# {appName}\n\nGenerated with Genkidama.\n");

    private static IEnumerable<GeneratedFile> ProjectFiles(string appName)
        => Enum.GetValues<GenkidamaProjectKind>().Select(kind => Project(appName, kind));

    private static GeneratedFile Project(string appName, GenkidamaProjectKind kind)
        => new($"src/{appName}.{kind}/{appName}.{kind}.csproj", ProjectXml());

    private static string ProjectXml()
        => "<Project Sdk=\"Microsoft.NET.Sdk\"><PropertyGroup><TargetFramework>net10.0</TargetFramework></PropertyGroup></Project>\n";
}
