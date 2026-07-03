namespace Genkidama.Cli;

/// <summary>
/// Creates generated block files.
/// </summary>
internal static class BlockTemplate
{
    /// <summary>
    /// Creates generated files for the selected block.
    /// </summary>
    /// <param name="appName">The generated application name.</param>
    /// <param name="blockName">The block name.</param>
    /// <returns>The generated files.</returns>
    internal static IReadOnlyList<GeneratedFile> Create(string appName, string blockName)
    {
        var name = BlockName.Normalize(blockName);
        return
        [
            Descriptor(appName, name),
            Starter(appName, name),
            Registration(appName, name),
            Readme(appName, name)
        ];
    }

    private static GeneratedFile Descriptor(string appName, string name)
        => new($"src/{appName}.Contracts/Blocks/{name}/{name}Descriptor.cs", DescriptorContent(appName, name));

    private static GeneratedFile Starter(string appName, string name)
        => new($"src/{appName}.Application/Blocks/{name}/{name}Starter.cs", StarterContent(appName, name));

    private static GeneratedFile Registration(string appName, string name)
        => new($"src/{appName}.Application/Blocks/{name}/{name}Registration.cs", RegistrationContent(appName, name));

    private static GeneratedFile Readme(string appName, string name)
        => new($"docs/blocks/{name}.md", $"# {name}\n\nGenerated block for {appName}.\n");

    private static string DescriptorContent(string appName, string name)
        => $"namespace {appName}.Contracts.Blocks.{name};\n\n" +
           $"/// <summary>\n/// Describes the {name} block.\n/// </summary>\n" +
           $"public sealed record {name}Descriptor(string Name);\n";

    private static string StarterContent(string appName, string name)
        => $"namespace {appName}.Application.Blocks.{name};\n\n" +
           $"/// <summary>\n/// Starts the {name} block.\n/// </summary>\n" +
           $"public sealed class {name}Starter\n{{\n}}\n";

    private static string RegistrationContent(string appName, string name)
        => $"namespace {appName}.Application.Blocks.{name};\n\n" +
           $"/// <summary>\n/// Registers the {name} block.\n/// </summary>\n" +
           $"public static class {name}Registration\n{{\n}}\n";
}
