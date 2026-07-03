namespace Genkidama.Blueprints;

public sealed class PlanCatalog
{
    private readonly IReadOnlyList<IPlanProvider> providers;

    public PlanCatalog(IReadOnlyList<IPlanProvider> providers)
        => this.providers = providers;

    public IReadOnlyList<IPlanProvider> Providers => providers;

    public PlanItem? Find(string id)
        => providers.SelectMany(provider => provider.GetItems()).FirstOrDefault(item => item.Id == id);
}
