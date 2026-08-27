using System;
using System.Collections.Generic;

interface IDocumentStore
{
    string Get(int id);
}

sealed class RemoteDocumentStore : IDocumentStore
{
    public int FetchCount { get; private set; }

    public string Get(int id)
    {
        FetchCount++;
        return $"doc({id})";
    }
}

sealed class DocumentStoreProxy : IDocumentStore
{
    private RemoteDocumentStore? _backend;
    private readonly Dictionary<int, string> _cache = new();

    public int BackendCount => _backend is null ? 0 : 1;
    public int FetchCount => _backend?.FetchCount ?? 0;

    public string Get(int id)
    {
        if (_cache.TryGetValue(id, out var cached))
        {
            return cached;
        }

        _backend ??= new RemoteDocumentStore();
        var value = _backend.Get(id);
        _cache[id] = value;
        return value;
    }
}

internal static class Program
{
    private static void Main()
    {
        var store = new DocumentStoreProxy();
        var first = store.Get(42);
        var second = store.Get(42);
        Console.WriteLine($"backend={store.BackendCount};fetches={store.FetchCount};first={first};second={second}");
    }
}
