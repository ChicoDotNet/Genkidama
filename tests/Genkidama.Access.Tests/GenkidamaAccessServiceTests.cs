using Genkidama.Access;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Access.Tests;

/// <summary>
/// Tests the access service.
/// </summary>
[TestClass]
public sealed class GenkidamaAccessServiceTests
{
    /// <summary>
    /// Verifies that direct capability allows access.
    /// </summary>
    [TestMethod]
    public void Check_WithDirectCapability_AllowsAccess()
    {
        var identity = new StandardIdentity("u1", "User", true, [], ["orders.read"]);
        var result = new GenkidamaAccessService().Check(identity, "orders.read", []);
        Assert.IsTrue(result.Succeeded);
        Assert.IsTrue(result.Value!.Allowed);
    }

    /// <summary>
    /// Verifies that role capability allows access.
    /// </summary>
    [TestMethod]
    public void Check_WithRoleCapability_AllowsAccess()
    {
        var identity = new StandardIdentity("u1", "User", true, ["operator"], []);
        var roles = new[] { new StandardRole("operator", ["orders.read"]) };
        var result = new GenkidamaAccessService().Check(identity, "orders.read", roles);
        Assert.IsTrue(result.Value!.Allowed);
    }

    /// <summary>
    /// Verifies that guest identity is rejected.
    /// </summary>
    [TestMethod]
    public void Check_WithGuestIdentity_RejectsAccess()
    {
        var result = new GenkidamaAccessService().Check(
            StandardIdentity.Anonymous(),
            "orders.read",
            []);
        Assert.IsFalse(result.Value!.Allowed);
    }
}
