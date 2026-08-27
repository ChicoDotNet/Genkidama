using System;
using System.Collections.Generic;

interface IRefundHandler
{
    IRefundHandler SetNext(IRefundHandler next);
    string Handle(int amount, List<string> visited);
}

abstract class RefundHandler : IRefundHandler
{
    private IRefundHandler? _next;

    protected abstract string Name { get; }
    protected abstract bool CanHandle(int amount);

    public IRefundHandler SetNext(IRefundHandler next)
    {
        _next = next;
        return next;
    }

    public string Handle(int amount, List<string> visited)
    {
        visited.Add(Name);
        if (CanHandle(amount))
        {
            return Name;
        }

        return _next?.Handle(amount, visited)
            ?? throw new InvalidOperationException("No handler accepted the request.");
    }
}

sealed class FaqHandler : RefundHandler
{
    protected override string Name => "faq";
    protected override bool CanHandle(int amount) => amount <= 50;
}

sealed class BillingHandler : RefundHandler
{
    protected override string Name => "billing";
    protected override bool CanHandle(int amount) => amount <= 500;
}

sealed class EscalationHandler : RefundHandler
{
    protected override string Name => "escalation";
    protected override bool CanHandle(int amount) => true;
}

static class Program
{
    public static void Main()
    {
        var faq = new FaqHandler();
        var billing = new BillingHandler();
        var escalation = new EscalationHandler();
        faq.SetNext(billing).SetNext(escalation);

        var visited = new List<string>();
        var handled = faq.Handle(250, visited);

        Console.WriteLine($"visited={string.Join(">", visited)};handled={handled};result=refund(250)");
    }
}
