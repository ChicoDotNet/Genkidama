using Genkidama.Cli;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Cli.Tests;

/// <summary>
/// Tests enum template generation.
/// </summary>
[TestClass]
public sealed class EnumTemplateTests
{
    /// <summary>
    /// Verifies that the template generates the expected domain enum file.
    /// </summary>
    [TestMethod]
    public void Create_WithValues_ReturnsDomainEnumFile()
    {
        var file = EnumTemplate.Create("SampleApp", "order_status", ["draft", "submitted"]);
        Assert.AreEqual("src/SampleApp.Domain/Enums/OrderStatus.cs", file.RelativePath);
        StringAssert.Contains(file.Content, "public enum OrderStatus");
        StringAssert.Contains(file.Content, "Submitted = 1");
    }
}
