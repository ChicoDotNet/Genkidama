namespace Genkidama.Blueprints;

public interface IPlanProvider
{
    string Name { get; }

    IReadOnlyList<PlanItem> GetItems();
}
