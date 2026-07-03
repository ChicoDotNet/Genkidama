namespace Genkidama.ConsoleReference;

/// <summary>
/// Describes a standard API problem for console clients.
/// </summary>
/// <param name="Code">The problem code.</param>
/// <param name="Message">The problem message.</param>
/// <param name="Target">The optional target.</param>
public sealed record StandardApiProblem(string Code, string Message, string? Target = null);
