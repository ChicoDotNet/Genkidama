using System;
using System.Collections.Generic;

public interface IPrototype<out T>
{
    T Clone();
}

public sealed class ServiceProfile : IPrototype<ServiceProfile>
{
    public string Name { get; set; }
    public List<string> Features { get; }

    public ServiceProfile(string name, IEnumerable<string> features)
    {
        Name = name;
        Features = new List<string>(features);
    }

    private ServiceProfile(ServiceProfile source)
    {
        Name = source.Name;
        Features = new List<string>(source.Features);
    }

    public ServiceProfile Clone() => new(this);

    public string Describe() => $"{Name}: {string.Join(",", Features)}";
}

public static class PrototypeExample
{
    public static void Main()
    {
        IPrototype<ServiceProfile> prototype = new ServiceProfile("orders", new[] { "metrics" });
        var original = (ServiceProfile)prototype;
        var canary = prototype.Clone();

        canary.Name = "orders-canary";
        canary.Features.Add("tracing");

        Console.WriteLine($"original={original.Describe()}");
        Console.WriteLine($"clone={canary.Describe()}");
    }
}
