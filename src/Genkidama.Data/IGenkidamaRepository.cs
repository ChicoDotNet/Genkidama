using Genkidama.Contracts;

namespace Genkidama.Data;

/// <summary>
/// Provides standard operations for entity collections.
/// </summary>
public interface IGenkidamaRepository<TEntity, TKey>
    where TEntity : IGenkidamaEntity<TKey>
{
    /// <summary>Finds one entity by identifier.</summary>
    StandardResult<TEntity> Find(TKey id);

    /// <summary>Lists entities using a standard query.</summary>
    StandardCollectionResult<TEntity> List(StandardQuery query);

    /// <summary>Adds an entity to the collection.</summary>
    StandardResult Add(TEntity entity);

    /// <summary>Removes an entity from the collection.</summary>
    StandardResult Remove(TKey id);
}
