using Genkidama.Contracts;

namespace Genkidama.Data;

/// <summary>
/// Provides standard operations for entity collections.
/// </summary>
/// <typeparam name="TEntity">The entity type.</typeparam>
/// <typeparam name="TKey">The entity identity type.</typeparam>
public interface IGenkidamaRepository<TEntity, in TKey>
    where TEntity : IGenkidamaEntity<TKey>
{
    /// <summary>
    /// Finds one entity by identifier.
    /// </summary>
    /// <param name="id">The entity identifier.</param>
    /// <returns>The entity result.</returns>
    StandardResult<TEntity> Find(TKey id);

    /// <summary>
    /// Lists entities using a standard query.
    /// </summary>
    /// <param name="query">The collection query.</param>
    /// <returns>The collection result.</returns>
    StandardCollectionResult<TEntity> List(StandardQuery query);

    /// <summary>
    /// Adds an entity to the collection.
    /// </summary>
    /// <param name="entity">The entity to add.</param>
    /// <returns>The operation result.</returns>
    StandardResult Add(TEntity entity);

    /// <summary>
    /// Removes an entity from the collection.
    /// </summary>
    /// <param name="id">The entity identifier.</param>
    /// <returns>The operation result.</returns>
    StandardResult Remove(TKey id);
}
