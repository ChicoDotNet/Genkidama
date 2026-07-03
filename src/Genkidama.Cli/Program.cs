namespace Genkidama.Cli;

/// <summary>
/// Provides the executable entry point for the Genkidama command line tool.
/// </summary>
internal static class Program
{
    /// <summary>
    /// Runs the command line tool.
    /// </summary>
    /// <param name="args">The command line arguments.</param>
    /// <returns>The process exit code.</returns>
    private static Task<int> Main(string[] args)
        => GenkidamaCommandRouter.ExecuteAsync(args, Console.Out);
}
