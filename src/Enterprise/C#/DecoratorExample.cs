using System;

public interface IComponent
{
    string Render();
}

public sealed class PlainMessage : IComponent
{
    public string Render() => "alert";
}

public abstract class ComponentDecorator(IComponent inner) : IComponent
{
    protected IComponent Inner { get; } = inner;
    public abstract string Render();
}

public sealed class AuditDecorator(IComponent inner) : ComponentDecorator(inner)
{
    public override string Render() => $"audit({Inner.Render()})";
}

public sealed class EncryptDecorator(IComponent inner) : ComponentDecorator(inner)
{
    public override string Render() => $"enc({Inner.Render()})";
}

public static class DecoratorExample
{
    public static void Main()
    {
        IComponent component = new PlainMessage();
        Console.WriteLine($"base={component.Render()}");
        Console.WriteLine($"audit={new AuditDecorator(component).Render()}");
        Console.WriteLine($"encrypted={new EncryptDecorator(component).Render()}");
        Console.WriteLine($"stacked={new AuditDecorator(new EncryptDecorator(component)).Render()}");
    }
}
