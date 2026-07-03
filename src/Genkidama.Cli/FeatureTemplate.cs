namespace Genkidama.Cli;

/// <summary>
/// Creates generated vertical slice feature files.
/// </summary>
internal static class FeatureTemplate
{
    /// <summary>
    /// Creates generated files for the selected feature.
    /// </summary>
    /// <param name="appName">The generated application name.</param>
    /// <param name="featureName">The feature name.</param>
    /// <returns>The generated files.</returns>
    internal static IReadOnlyList<GeneratedFile> Create(string appName, string featureName)
    {
        var name = FeatureName.Normalize(featureName);
        return
        [
            Dto(appName, name),
            Command(appName, name),
            Query(appName, name),
            CommandHandler(appName, name),
            QueryHandler(appName, name),
            Readme(appName, name)
        ];
    }

    private static GeneratedFile Dto(string appName, string name)
        => new($"src/{appName}.Contracts/Features/{name}/{name}Dto.cs", DtoContent(appName, name));

    private static GeneratedFile Command(string appName, string name)
        => new($"src/{appName}.Application/Features/{name}/{name}Command.cs", CommandContent(appName, name));

    private static GeneratedFile Query(string appName, string name)
        => new($"src/{appName}.Application/Features/{name}/{name}Query.cs", QueryContent(appName, name));

    private static GeneratedFile CommandHandler(string appName, string name)
        => new($"src/{appName}.Application/Features/{name}/{name}CommandHandler.cs", CommandHandlerContent(appName, name));

    private static GeneratedFile QueryHandler(string appName, string name)
        => new($"src/{appName}.Application/Features/{name}/{name}QueryHandler.cs", QueryHandlerContent(appName, name));

    private static GeneratedFile Readme(string appName, string name)
        => new($"docs/features/{name}.md", $"# {name}\n\nGenerated feature for {appName}.\n");

    private static string DtoContent(string appName, string name)
        => $"namespace {appName}.Contracts.Features.{name};\n\n" +
           $"/// <summary>\n/// Represents {name} data.\n/// </summary>\n" +
           $"public sealed record {name}Dto(string Name);\n";

    private static string CommandContent(string appName, string name)
        => $"using {appName}.Contracts.Features.{name};\n\n" +
           $"namespace {appName}.Application.Features.{name};\n\n" +
           $"/// <summary>\n/// Requests a {name} change.\n/// </summary>\n" +
           $"public sealed record {name}Command({name}Dto Value);\n";

    private static string QueryContent(string appName, string name)
        => $"namespace {appName}.Application.Features.{name};\n\n" +
           $"/// <summary>\n/// Requests {name} data.\n/// </summary>\n" +
           $"public sealed record {name}Query(string Name);\n";

    private static string CommandHandlerContent(string appName, string name)
        => $"namespace {appName}.Application.Features.{name};\n\n" +
           $"/// <summary>\n/// Handles {name} commands.\n/// </summary>\n" +
           $"public sealed class {name}CommandHandler\n{{\n}}\n";

    private static string QueryHandlerContent(string appName, string name)
        => $"namespace {appName}.Application.Features.{name};\n\n" +
           $"/// <summary>\n/// Handles {name} queries.\n/// </summary>\n" +
           $"public sealed class {name}QueryHandler\n{{\n}}\n";
}
