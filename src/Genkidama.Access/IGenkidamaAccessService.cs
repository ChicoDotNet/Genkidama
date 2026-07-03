using Genkidama.Contracts;

namespace Genkidama.Access;

/// <summary>
/// Evaluates whether an identity can use a capability.
/// </summary>
public interface IGenkidamaAccessService
{
    /// <summary>
    /// Evaluates access for one capability.
    /// </summary>
    /// <param name="identity">The identity.</param>
    /// <param name="capability">The required capability.</param>
    /// <param name="roles">The known roles.</param>
    /// <returns>The access decision.</returns>
    StandardResult<StandardAccessDecision> Check(
        StandardIdentity identity,
        string capability,
        IEnumerable<StandardRole> roles);
}
