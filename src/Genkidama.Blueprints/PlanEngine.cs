namespace Genkidama.Blueprints;

public sealed class PlanEngine
{
    private readonly PlanCatalog catalog;
    private readonly PlanRenderer renderer;

    public PlanEngine(PlanCatalog catalog, PlanRenderer renderer)
    {
        this.catalog = catalog;
        this.renderer = renderer;
    }

    public BlueprintFile? Render(string id, PlanContext context)
    {
        var item = catalog.Find(id);
        return item is null ? null : renderer.Render(item, context);
    }
}
