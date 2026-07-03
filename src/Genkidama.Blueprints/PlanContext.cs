namespace Genkidama.Blueprints;

public sealed class PlanContext
{
    private readonly Dictionary<string, string> values = new(StringComparer.OrdinalIgnoreCase);

    public IReadOnlyDictionary<string, string> Values => values;

    public PlanContext With(string key, string value)
    {
        values[key] = value;
        return this;
    }
}
