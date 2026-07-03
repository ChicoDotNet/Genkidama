namespace Genkidama.Access;

/// <summary>
/// Describes the result of an access check.
/// </summary>
/// <param name="Allowed">Whether access is allowed.</param>
/// <param name="Reason">The decision reason.</param>
public sealed record StandardAccessDecision(bool Allowed, string Reason)
{
    /// <summary>
    /// Creates an allowed decision.
    /// </summary>
    /// <returns>The allowed decision.</returns>
    public static StandardAccessDecision Allow()
        => new(true, "Allowed");

    /// <summary>
    /// Creates a rejected decision.
    /// </summary>
    /// <param name="reason">The rejection reason.</param>
    /// <returns>The rejected decision.</returns>
    public static StandardAccessDecision Reject(string reason)
        => new(false, reason);
}
