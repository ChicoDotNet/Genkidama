using Microsoft.AspNetCore.Http;

namespace Genkidama.Http;

/// <summary>
/// Adds the Genkidama trace identifier header to HTTP responses.
/// </summary>
public sealed class GenkidamaTraceMiddleware
{
    private readonly RequestDelegate next;

    /// <summary>
    /// Initializes a new instance of the <see cref="GenkidamaTraceMiddleware"/> class.
    /// </summary>
    /// <param name="next">The next request delegate.</param>
    public GenkidamaTraceMiddleware(RequestDelegate next)
        => this.next = next;

    /// <summary>
    /// Processes the current HTTP request.
    /// </summary>
    /// <param name="context">The HTTP context.</param>
    /// <returns>A task representing the asynchronous operation.</returns>
    public async Task InvokeAsync(HttpContext context)
    {
        var traceId = GenkidamaTraceIdentifier.Get(context);
        context.Response.Headers[GenkidamaHttpNames.TraceIdHeader] = traceId;
        await next(context);
    }
}
