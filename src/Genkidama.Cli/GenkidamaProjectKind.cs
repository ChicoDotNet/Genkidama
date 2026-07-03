namespace Genkidama.Cli;

/// <summary>
/// Identifies a generated Genkidama project role.
/// </summary>
internal enum GenkidamaProjectKind
{
    /// <summary>The HTTP API project.</summary>
    Api,

    /// <summary>The application contracts project.</summary>
    Contracts,

    /// <summary>The application orchestration project.</summary>
    Application,

    /// <summary>The domain model project.</summary>
    Domain,

    /// <summary>The persistence project.</summary>
    Persistence
}
