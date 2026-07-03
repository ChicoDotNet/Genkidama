namespace Genkidama.Access;

/// <summary>
/// Describes the current application identity.
/// </summary>
/// <param name="Id">The identity identifier.</param>
/// <param name="DisplayName">The display name.</param>
/// <param name="IsAuthenticated">Whether the identity is authenticated.</param>
/// <param name="Roles">The assigned roles.</param>
/// <param name="Capabilities">The direct capabilities.</param>
public sealed record StandardIdentity(
    string Id,
    string DisplayName,
    bool IsAuthenticated,
    IReadOnlyList<string> Roles,
    IReadOnlyList<string> Capabilities)
{
    /// <summary>
    /// Creates an anonymous identity.
    /// </summary>
    /// <returns>The anonymous identity.</returns>
    public static StandardIdentity Anonymous()
        => new(string.Empty, "Anonymous", false, [], []);

    /// <summary>
    /// Checks whether this identity has a role.
    /// </summary>
    /// <param name="role">The role name.</param>
    /// <returns>True when the identity has the role.</returns>
    public bool HasRole(string role)
        => Roles.Contains(role, StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// Checks whether this identity has a direct capability.
    /// </summary>
    /// <param name="capability">The capability name.</param>
    /// <returns>True when the identity has the capability.</returns>
    public bool HasCapability(string capability)
        => Capabilities.Contains(capability, StringComparer.OrdinalIgnoreCase);
}
