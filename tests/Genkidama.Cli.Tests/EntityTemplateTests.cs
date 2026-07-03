using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests entity template generation.
/// </summary>
[TestClass]
public sealed class EntityTemplateTests
{
    /// <summary>
    /// Verifies that the template generates the expected domain entity path.
    /// </summary>
    [TestMethod]
    public void Create_WithEntityName_ReturnsDomainEntityFile()
    {
        var file = EntityTemplate.Create("SampleApp", "customer");
        Assert.AreEqual("src/SampleApp.Domain/Entities/Customer.cs", file.RelativePath);
        StringAssert.Contains(file.Content, "public sealed class Customer");
    }
}
