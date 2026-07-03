namespace Genkidama.Contracts;

/// <summary>
/// Describes a normalized problem returned by a Genkidama operation.
/// </summary>
/// <param name="Code">The stable problem code.</param>
/// <param name="Message">The human readable problem message.</param>
/// <param name="Target">The optional affected member or resource.</param>
public sealed record StandardProblem(
    string Code,
    string Message,
    string? Target = null)
{
    /// <summary>
    /// Creates a validation problem.
    /// </summary>
    /// <param name="message">The validation message.</param>
    /// <param name="target">The optional affected member.</param>
    /// <returns>The validation problem.</returns>
    public static StandardProblem Validation(string message, string? target = null)
        => new("validation_error", message, target);
}
