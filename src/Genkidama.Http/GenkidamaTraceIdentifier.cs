using Microsoft.AspNetCore.Http;

namespace Genkidama.Http;

/// <summary>
/// Provides helpers for stable HTTP trace identifiers.
/// </summary>
public static class GenkidamaTraceIdentifier
{
    /// <summary>
    /// Gets the effective trace identifier for the HTTP context.
    /// </summary>
    /// <param name="context">The HTTP context.</param>
    /// <returns>The effective trace identifier.</returns>
    public static string Get(HttpContext context)
    {
        ArgumentNullException.ThrowIfNull(context);
        return string.IsNullOrWhiteSpace(context.TraceIdentifier)
            ? Guid.NewGuid().ToString("N")
            : context.TraceIdentifier;
    }
}
