using Genkidama.Contracts;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Contracts.Tests;

/// <summary>
/// Tests the normalized standard result contract.
/// </summary>
[TestClass]
public sealed class StandardResultTests
{
    /// <summary>
    /// Verifies that a successful result has no problem.
    /// </summary>
    [TestMethod]
    public void Success_ReturnsSucceededResult()
    {
        var result = StandardResult.Success();
        Assert.IsTrue(result.Succeeded);
        Assert.IsNull(result.Problem);
    }

    /// <summary>
    /// Verifies that a failed result carries its problem.
    /// </summary>
    [TestMethod]
    public void Failure_WithProblem_ReturnsFailedResult()
    {
        var problem = StandardProblem.Validation("Name is required.", "name");
        var result = StandardResult.Failure(problem);
        Assert.IsFalse(result.Succeeded);
        Assert.AreEqual(problem, result.Problem);
    }
}
