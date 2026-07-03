using System.Xml.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests .NET tool package metadata.
/// </summary>
[TestClass]
public sealed class DotNetToolPackageMetadataTests
{
    /// <summary>
    /// Verifies that the CLI project is configured as a .NET tool.
    /// </summary>
    [TestMethod]
    public void CliProject_WithPackageMetadata_IsDotNetTool()
    {
        var project = XDocument.Load(ProjectPath());
        Assert.AreEqual("true", Value(project, "PackAsTool"));
        Assert.AreEqual("genkidama", Value(project, "ToolCommandName"));
        Assert.AreEqual("Genkidama.Cli", Value(project, "PackageId"));
    }

    private static string Value(XDocument document, string name)
        => document.Descendants(name).Single().Value;

    private static string ProjectPath()
        => Path.Combine(RepositoryRoot(), "src", "Genkidama.Cli", "Genkidama.Cli.csproj");

    private static string RepositoryRoot()
    {
        var directory = new DirectoryInfo(AppContext.BaseDirectory);
        while (directory is not null && !File.Exists(Path.Combine(directory.FullName, "Genkidama.slnx")))
        {
            directory = directory.Parent;
        }

        return directory?.FullName ?? throw new DirectoryNotFoundException("Repository root was not found.");
    }
}
