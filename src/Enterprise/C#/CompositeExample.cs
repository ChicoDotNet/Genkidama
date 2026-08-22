using System;
using System.Collections.Generic;
using System.Linq;

public interface IComponent
{
    int Size();
}

public sealed class FileLeaf(int size) : IComponent
{
    public int Size() => size;
}

public sealed class FolderComposite(params IComponent[] children) : IComponent
{
    private readonly List<IComponent> _children = [.. children];

    public int Size() => _children.Sum(child => child.Size());
}

public static class CompositeExample
{
    public static void Main()
    {
        IComponent readme = new FileLeaf(2);
        IComponent docs = new FolderComposite(new FileLeaf(3), new FileLeaf(5));
        IComponent root = new FolderComposite(readme, docs);

        Console.WriteLine($"leaf={readme.Size()}");
        Console.WriteLine($"docs={docs.Size()}");
        Console.WriteLine($"root={root.Size()}");
    }
}
