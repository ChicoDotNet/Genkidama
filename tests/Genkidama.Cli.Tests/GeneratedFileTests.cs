using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests generated file writing behavior.
/// </summary>
[TestClass]
public sealed class GeneratedFileTests
{
    /// <summary>
    /// Verifies that nested output directories are created before writing.
    /// </summary>
    [TestMethod]
    public void WriteTo_WithNestedPath_CreatesDirectories()
    {
        using var workspace = new TemporaryWorkspace();
        var file = new GeneratedFile("src/Sample/Nested/File.txt", "Hello");
        file.WriteTo(workspace.Root);
        Assert.AreEqual("Hello", File.ReadAllText(workspace.PathOf("src", "Sample", "Nested", "File.txt")));
    }
}
