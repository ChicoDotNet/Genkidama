using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests block template generation.
/// </summary>
[TestClass]
public sealed class BlockTemplateTests
{
    /// <summary>
    /// Verifies that the template generates expected block files.
    /// </summary>
    [TestMethod]
    public void Create_WithBlockName_ReturnsBlockFiles()
    {
        var files = BlockTemplate.Create("SampleApp", "audit");
        Assert.AreEqual(4, files.Count);
        Assert.IsTrue(files.Any(file => file.RelativePath.EndsWith("AuditDescriptor.cs")));
        Assert.IsTrue(files.Any(file => file.RelativePath.EndsWith("AuditStarter.cs")));
        Assert.IsTrue(files.Any(file => file.RelativePath.EndsWith("AuditRegistration.cs")));
    }
}
