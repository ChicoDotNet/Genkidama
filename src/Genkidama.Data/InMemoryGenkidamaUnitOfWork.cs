using Genkidama.Contracts;

namespace Genkidama.Data;

/// <summary>
/// Provides a no-op unit of work for tests and generated samples.
/// </summary>
public sealed class InMemoryGenkidamaUnitOfWork : IGenkidamaUnitOfWork
{
    private int saveCount;

    /// <summary>
    /// Gets the number of times changes were saved.
    /// </summary>
    public int SaveCount => saveCount;

    /// <inheritdoc />
    public StandardResult<int> SaveChanges()
    {
        saveCount++;
        return StandardResult<int>.Success(saveCount);
    }
}
