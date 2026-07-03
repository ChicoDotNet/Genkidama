using Genkidama.Events;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Events.Tests;

/// <summary>
/// Tests the standard event contract.
/// </summary>
[TestClass]
public sealed class StandardEventTests
{
    /// <summary>
    /// Verifies that created events have identity and name.
    /// </summary>
    [TestMethod]
    public void Create_WithName_ReturnsEvent()
    {
        var eventItem = StandardEvent.Create("thing.created");
        Assert.AreEqual("thing.created", eventItem.Name);
        Assert.AreNotEqual(Guid.Empty, eventItem.Id);
        Assert.IsTrue(eventItem.OccurredUtc <= DateTimeOffset.UtcNow);
    }
}
