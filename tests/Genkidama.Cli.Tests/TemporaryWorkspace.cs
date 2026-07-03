namespace Genkidama.Cli.Tests;

/// <summary>
/// Provides an isolated temporary directory for file based tests.
/// </summary>
internal sealed class TemporaryWorkspace : IDisposable
{
    /// <summary>
    /// Initializes a new instance of the <see cref="TemporaryWorkspace"/> class.
    /// </summary>
    public TemporaryWorkspace(string? leafName = null)
    {
        var root = Path.Combine(Path.GetTempPath(), "genkidama-tests", Guid.NewGuid().ToString("N"));
        Root = leafName is null ? root : Path.Combine(root, leafName);
        Directory.CreateDirectory(Root);
    }

    /// <summary>
    /// Gets the workspace root path.
    /// </summary>
    public string Root { get; }

    /// <summary>
    /// Combines a path under the workspace root.
    /// </summary>
    public string PathOf(params string[] parts)
        => Path.Combine([Root, .. parts]);

    /// <inheritdoc />
    public void Dispose()
    {
        if (Directory.Exists(Root))
        {
            Directory.Delete(Root, true);
        }
    }
}
