using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests the new solution template.
/// </summary>
[TestClass]
public sealed class NewSolutionTemplateTests
{
    /// <summary>
    /// Verifies that the template creates the expected project files.
    /// </summary>
    [TestMethod]
    public void Create_WithAppName_ReturnsBackendProjects()
    {
        var files = NewSolutionTemplate.Create("SampleApp");
        Assert.IsTrue(files.Any(file => file.RelativePath == "src/SampleApp.Api/SampleApp.Api.csproj"));
        Assert.IsTrue(files.Any(file => file.RelativePath == "src/SampleApp.Contracts/SampleApp.Contracts.csproj"));
    }
}
