using Genkidama.Events;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Events.Tests;

/// <summary>
/// Tests the in-memory notification channel.
/// </summary>
[TestClass]
public sealed class InMemoryGenkidamaNotificationChannelTests
{
    /// <summary>
    /// Verifies that sent notifications are retained.
    /// </summary>
    [TestMethod]
    public async Task SendAsync_WithNotification_StoresNotification()
    {
        var channel = new InMemoryGenkidamaNotificationChannel();
        var notification = StandardNotification.Create("demo", "memory", "hello");
        var result = await channel.SendAsync(notification);
        Assert.IsTrue(result.Succeeded);
        Assert.AreEqual(1, channel.Sent.Count);
        Assert.AreEqual(notification, channel.Sent[0]);
    }
}
