using Genkidama.Contracts;

namespace Genkidama.Access;

/// <summary>
/// Evaluates access using capabilities and roles.
/// </summary>
public sealed class GenkidamaAccessService : IGenkidamaAccessService
{
    /// <inheritdoc />
    public StandardResult<StandardAccessDecision> Check(
        StandardIdentity identity,
        string capability,
        IEnumerable<StandardRole> roles)
    {
        var allowed = identity.IsAuthenticated && CanUse(identity, capability, roles);
        var decision = allowed
            ? StandardAccessDecision.Allow()
            : StandardAccessDecision.Reject("Not allowed.");
        return StandardResult<StandardAccessDecision>.Success(decision);
    }

    private static bool CanUse(
        StandardIdentity identity,
        string capability,
        IEnumerable<StandardRole> roles)
        => identity.HasCapability(capability) || roles.Any(role => identity.HasRole(role.Name) && role.HasCapability(capability));
}
