using Genkidama.Contracts;

namespace Genkidama.Data;

/// <summary>
/// Coordinates a group of data operations as one unit.
/// </summary>
public interface IGenkidamaUnitOfWork
{
    /// <summary>
    /// Saves pending data changes.
    /// </summary>
    /// <returns>The number of saved changes.</returns>
    StandardResult<int> SaveChanges();
}
