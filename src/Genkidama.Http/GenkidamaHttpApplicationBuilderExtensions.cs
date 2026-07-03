using Microsoft.AspNetCore.Builder;

namespace Genkidama.Http;

/// <summary>
/// Provides application builder extensions for the Genkidama HTTP layer.
/// </summary>
public static class GenkidamaHttpApplicationBuilderExtensions
{
    /// <summary>
    /// Adds Genkidama trace identifier middleware to the application pipeline.
    /// </summary>
    /// <param name="app">The application builder.</param>
    /// <returns>The application builder.</returns>
    public static IApplicationBuilder UseGenkidamaTraceIdentifier(this IApplicationBuilder app)
    {
        ArgumentNullException.ThrowIfNull(app);
        return app.UseMiddleware<GenkidamaTraceMiddleware>();
    }
}
