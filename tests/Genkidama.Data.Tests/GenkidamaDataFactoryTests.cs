using Genkidama.Data;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Data.Tests;

/// <summary>
/// Tests the Genkidama data factory.
/// </summary>
[TestClass]
public sealed class GenkidamaDataFactoryTests
{
    /// <summary>
    /// Verifies that valid options create a descriptor.
    /// </summary>
    [TestMethod]
    public void Create_WithValidOptions_ReturnsDescriptor()
    {
        var factory = new GenkidamaDataFactory();
        var result = factory.Create(GenkidamaDataOptions.ForSqlite("Data Source=app.db"));
        Assert.IsTrue(result.Succeeded);
        Assert.AreEqual(GenkidamaDatabaseProvider.Sqlite, result.Value!.Provider);
        Assert.IsTrue(result.Value.Schemas.Contains(GenkidamaDataSchemas.Business));
    }

    /// <summary>
    /// Verifies that an empty connection string returns a failure.
    /// </summary>
    [TestMethod]
    public void Create_WithEmptyConnectionString_ReturnsFailure()
    {
        var factory = new GenkidamaDataFactory();
        var options = new GenkidamaDataOptions(GenkidamaDatabaseProvider.SqlServer, string.Empty);
        var result = factory.Create(options);
        Assert.IsFalse(result.Succeeded);
        Assert.IsNotNull(result.Problem);
    }
}
