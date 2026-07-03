using Genkidama.Contracts;

namespace Genkidama.Data;

/// <summary>
/// Creates configured Genkidama data descriptors.
/// </summary>
public interface IGenkidamaDataFactory
{
    /// <summary>
    /// Creates a descriptor from options.
    /// </summary>
    /// <param name="options">The data options.</param>
    /// <returns>The descriptor result.</returns>
    StandardResult<GenkidamaDataDescriptor> Create(GenkidamaDataOptions options);
}
