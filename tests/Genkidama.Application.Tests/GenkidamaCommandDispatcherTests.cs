using Genkidama.Application;
using Genkidama.Contracts;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Application.Tests;

/// <summary>
/// Tests the command dispatcher.
/// </summary>
[TestClass]
public sealed class GenkidamaCommandDispatcherTests
{
    /// <summary>
    /// Verifies that a command is sent to its handler.
    /// </summary>
    [TestMethod]
    public async Task SendAsync_WithHandler_ReturnsHandlerResult()
    {
        var dispatcher = new GenkidamaCommandDispatcher();
        var result = await dispatcher.SendAsync(new CreateThing("demo"), new Handler(), []);
        Assert.IsTrue(result.Succeeded);
        Assert.AreEqual("created:demo", result.Value);
    }

    private sealed record CreateThing(string Name) : IGenkidamaCommand<string>;

    private sealed class Handler : IGenkidamaCommandHandler<CreateThing, string>
    {
        public Task<StandardResult<string>> HandleAsync(
            CreateThing command,
            CancellationToken cancellationToken = default)
            => Task.FromResult(StandardResult<string>.Success($"created:{command.Name}"));
    }
}
