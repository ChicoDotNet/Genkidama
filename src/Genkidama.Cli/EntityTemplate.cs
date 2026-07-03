namespace Genkidama.Cli;

/// <summary>
/// Creates generated entity files.
/// </summary>
internal static class EntityTemplate
{
    /// <summary>
    /// Creates the generated entity source file.
    /// </summary>
    /// <param name="appName">The generated application name.</param>
    /// <param name="entityName">The entity name.</param>
    /// <returns>The generated file.</returns>
    internal static GeneratedFile Create(string appName, string entityName)
    {
        var name = EntityName.Normalize(entityName);
        return new GeneratedFile(
            $"src/{appName}.Domain/Entities/{name}.cs",
            Content(appName, name));
    }

    private static string Content(string appName, string entityName)
        => $$"""
        namespace {{appName}}.Domain.Entities;

        /// <summary>
        /// Represents the {{entityName}} entity.
        /// </summary>
        public sealed class {{entityName}}
        {
            /// <summary>
            /// Gets or initializes the entity identifier.
            /// </summary>
            public Guid Id { get; init; } = Guid.NewGuid();
        }
        """ + "\n";
}
