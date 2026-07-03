using Genkidama.MauiReference;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.MauiReference.Tests;

/// <summary>
/// Tests async command behavior.
/// </summary>
[TestClass]
public sealed class AsyncCommandTests
{
    /// <summary>
    /// Verifies that the command executes the supplied task.
    /// </summary>
    [TestMethod]
    public async Task RunAsync_WithAction_ExecutesAction()
    {
        var executed = false;
        var command = new AsyncCommand(() =>
        {
            executed = true;
            return Task.CompletedTask;
        });

        await command.RunAsync();
        Assert.IsTrue(executed);
    }
}
