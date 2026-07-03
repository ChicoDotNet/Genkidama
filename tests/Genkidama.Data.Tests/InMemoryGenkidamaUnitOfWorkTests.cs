using Genkidama.Data;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Data.Tests;

/// <summary>
/// Tests the in-memory work coordinator.
/// </summary>
[TestClass]
public sealed class InMemoryGenkidamaUnitOfWorkTests
{
    /// <summary>
    /// Verifies that save changes returns the save count.
    /// </summary>
    [TestMethod]
    public void SaveChanges_WhenCalled_ReturnsSaveCount()
    {
        var work = new InMemoryGenkidamaUnitOfWork();
        var result = work.SaveChanges();
        Assert.IsTrue(result.Succeeded);
        Assert.AreEqual(1, result.Value);
        Assert.AreEqual(1, work.SaveCount);
    }
}
