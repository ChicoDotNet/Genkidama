using Genkidama.Contracts;

namespace Genkidama.Data;

/// <summary>
/// Provides an in-memory repository for tests and generated samples.
/// </summary>
/// <typeparam name="TEntity">The entity type.</typeparam>
/// <typeparam name="TKey">The entity identity type.</typeparam>
public sealed class InMemoryGenkidamaRepository<TEntity, TKey> : IGenkidamaRepository<TEntity, TKey>
    where TEntity : IGenkidamaEntity<TKey>
    where TKey : notnull
{
    private readonly Dictionary<TKey, TEntity> items = [];

    /// <inheritdoc />
    public StandardResult Add(TEntity entity)
    {
        items[entity.Id] = entity;
        return StandardResult.Success();
    }

    /// <inheritdoc />
    public StandardResult<TEntity> Find(TKey id)
        => items.TryGetValue(id, out var entity)
            ? StandardResult<TEntity>.Success(entity)
            : StandardResult<TEntity>.Failure(StandardProblem.Validation("Entity was not found."));

    /// <inheritdoc />
    public StandardCollectionResult<TEntity> List(StandardQuery query)
        => StandardCollectionResult<TEntity>.From(Page(query), items.Count, query);

    /// <inheritdoc />
    public StandardResult Remove(TKey id)
        => items.Remove(id) ? StandardResult.Success() : StandardResult.Failure(NotFound());

    private IReadOnlyList<TEntity> Page(StandardQuery query)
        => items.Values.Skip(query.Skip).Take(query.PageSize).ToArray();

    private static StandardProblem NotFound()
        => StandardProblem.Validation("Entity was not found.");
}
