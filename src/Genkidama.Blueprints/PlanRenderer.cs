namespace Genkidama.Blueprints;

public sealed class PlanRenderer
{
    public BlueprintFile Render(PlanItem item, PlanContext context)
        => new(RenderText(item.RelativePath, context), RenderText(item.Body, context));

    private static string RenderText(string value, PlanContext context)
    {
        var result = value;
        foreach (var pair in context.Values)
        {
            result = result.Replace("{{" + pair.Key + "}}", pair.Value, StringComparison.OrdinalIgnoreCase);
        }

        return result;
    }
}
