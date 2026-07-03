namespace Genkidama.Access;

/// <summary>
/// Describes a named group of capabilities.
/// </summary>
/// <param name="Name">The role name.</param>
/// <param name="Capabilities">The capability names.</param>
public sealed record StandardRole(
    string Name,
    IReadOnlyList<string> Capabilities)
{
    /// <summary>
    /// Checks whether this role contains a capability.
    /// </summary>
    /// <param name="capability">The capability name.</param>
    /// <returns>True when the role contains the capability.</returns>
    public bool HasCapability(string capability)
        => Capabilities.Contains(capability, StringComparer.OrdinalIgnoreCase);
}
