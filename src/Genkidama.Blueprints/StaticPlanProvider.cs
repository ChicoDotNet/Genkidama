namespace Genkidama.Blueprints;

public sealed class StaticPlanProvider : IPlanProvider
{
    private readonly IReadOnlyList<PlanItem> items;

    public StaticPlanProvider(string name, IReadOnlyList<PlanItem> items)
    {
        Name = name;
        this.items = items;
    }

    public string Name { get; }

    public IReadOnlyList<PlanItem> GetItems()
        => items;
}
