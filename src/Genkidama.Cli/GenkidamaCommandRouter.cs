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
        if (IsNew(args))
        {
            return RunNewAsync(args[1], writer);
        }

        return WriteKnownAsync(args, writer);
    }

    private static bool IsNew(string[] args)
        => args.Length == 2 && args[0] == "new";

    private static Task<int> RunNewAsync(string appName, TextWriter writer)
        => GenkidamaNewCommand.ExecuteAsync(
            new NewSolutionOptions(appName, Environment.CurrentDirectory),
            writer);

    private static Task<int> WriteKnownAsync(string[] args, TextWriter writer)
    {
        writer.WriteLine(ResolveText(args));
        return Task.FromResult(0);
    }

    private static string ResolveText(string[] args)
        => args.Length == 1 && args[0] == "--version"
            ? GenkidamaInfo.SemanticIdentifier
            : GenkidamaHelp.Text;
}
