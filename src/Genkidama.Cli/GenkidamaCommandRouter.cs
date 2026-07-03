namespace Genkidama.Cli;

/// <summary>
/// Routes command line arguments to Genkidama CLI behaviors.
/// </summary>
internal static class GenkidamaCommandRouter
{
    /// <summary>
    /// Executes the command selected by the supplied arguments.
    /// </summary>
    /// <param name="args">The command line arguments.</param>
    /// <param name="writer">The output writer.</param>
    /// <returns>The process exit code.</returns>
    internal static Task<int> ExecuteAsync(string[] args, TextWriter writer)
    {
        var text = ResolveText(args);
        writer.WriteLine(text);
        return Task.FromResult(0);
    }

    private static string ResolveText(string[] args)
        => args.Length == 1 && args[0] == "--version"
            ? GenkidamaInfo.SemanticIdentifier
            : GenkidamaHelp.Text;
}
