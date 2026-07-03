using Genkidama.Contracts;

namespace Genkidama.Data;

/// <summary>
/// Creates configured Genkidama data descriptors.
/// </summary>
public sealed class GenkidamaDataFactory : IGenkidamaDataFactory
{
    private static readonly string[] StandardSchemas =
    [
        GenkidamaDataSchemas.Audit,
        GenkidamaDataSchemas.Business,
        GenkidamaDataSchemas.Enum,
        GenkidamaDataSchemas.Security,
        GenkidamaDataSchemas.Utility
    ];

    /// <inheritdoc />
    public StandardResult<GenkidamaDataDescriptor> Create(GenkidamaDataOptions options)
    {
        if (string.IsNullOrWhiteSpace(options.ConnectionString))
        {
            var problem = StandardProblem.Validation("Connection string is required.");
            return StandardResult<GenkidamaDataDescriptor>.Failure(problem);
        }

        var descriptor = new GenkidamaDataDescriptor(options.Provider, options.ConnectionString, StandardSchemas);
        return StandardResult<GenkidamaDataDescriptor>.Success(descriptor);
    }
}
