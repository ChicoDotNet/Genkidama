using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests feature template generation.
/// </summary>
[TestClass]
public sealed class FeatureTemplateTests
{
    /// <summary>
    /// Verifies that the template generates expected feature files.
    /// </summary>
    [TestMethod]
    public void Create_WithFeatureName_ReturnsFeatureFiles()
    {
        var files = FeatureTemplate.Create("SampleApp", "create_order");
        Assert.AreEqual(6, files.Count);
        Assert.IsTrue(files.Any(file => file.RelativePath.EndsWith("CreateOrderDto.cs")));
        Assert.IsTrue(files.Any(file => file.RelativePath.EndsWith("CreateOrderCommand.cs")));
        Assert.IsTrue(files.Any(file => file.RelativePath.EndsWith("CreateOrderQuery.cs")));
    }
}
