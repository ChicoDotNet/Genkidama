using Genkidama.Contracts;
using Genkidama.Data;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Genkidama.Data.Tests;

/// <summary>
/// Tests the in-memory repository implementation.
/// </summary>
[TestClass]
public sealed class InMemoryGenkidamaRepositoryTests
{
    /// <summary>
    /// Verifies that an added entity can be found by identity.
    /// </summary>
    [TestMethod]
    public void Find_AfterAdd_ReturnsEntity()
    {
        var repository = new InMemoryGenkidamaRepository<TestEntity<Guid>, Guid>();
        var entity = new TestEntity<Guid>(Guid.NewGuid(), "Alpha");
        repository.Add(entity);
        var result = repository.Find(entity.Id);
        Assert.IsTrue(result.Succeeded);
        Assert.AreEqual(entity, result.Value);
    }

    /// <summary>
    /// Verifies that query paging is applied to listed entities.
    /// </summary>
    [TestMethod]
    public void List_WithSecondPage_ReturnsPagedItems()
    {
        var repository = new InMemoryGenkidamaRepository<TestEntity<int>, int>();
        repository.Add(new(1, "One"));
        repository.Add(new(2, "Two"));
        var result = repository.List(new StandardQuery(2, 1));
        Assert.AreEqual(1, result.Items.Count);
        Assert.AreEqual(2, result.Items[0].Id);
    }

    private sealed record TestEntity<TKey>(TKey Id, string Name) : IGenkidamaEntity<TKey>;
}
