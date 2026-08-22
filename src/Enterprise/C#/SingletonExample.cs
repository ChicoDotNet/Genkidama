using System;

public sealed class ProcessRegistry
{
    private static readonly Lazy<ProcessRegistry> LazyInstance = new(() => new ProcessRegistry());
    private ProcessRegistry() { }
    public static ProcessRegistry Instance => LazyInstance.Value;
    public int Count { get; private set; }
    public void Increment() => Count++;
}

public static class SingletonExample
{
    public static void Main()
    {
        var first = ProcessRegistry.Instance;
        var second = ProcessRegistry.Instance;
        first.Increment();
        Console.WriteLine($"same={ReferenceEquals(first, second).ToString().ToLowerInvariant()}");
        Console.WriteLine($"count={second.Count}");
    }
}
