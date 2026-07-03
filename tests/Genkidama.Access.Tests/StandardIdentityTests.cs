using Genkidama.Access;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Access.Tests;

/// <summary>
/// Tests standard identity behavior.
/// </summary>
[TestClass]
public sealed class StandardIdentityTests
{
    /// <summary>
    /// Verifies that roles are matched without case sensitivity.
    /// </summary>
    [TestMethod]
    public void HasRole_WithDifferentCase_ReturnsTrue()
    {
        var identity = new StandardIdentity("u1", "User", true, ["Admin"], []);
        Assert.IsTrue(identity.HasRole("admin"));
    }

    /// <summary>
    /// Verifies that the guest identity is not active.
    /// </summary>
    [TestMethod]
    public void Anonymous_ReturnsGuestIdentity()
    {
        var identity = StandardIdentity.Anonymous();
        Assert.IsFalse(identity.IsAuthenticated);
        Assert.AreEqual("Anonymous", identity.DisplayName);
    }
}
