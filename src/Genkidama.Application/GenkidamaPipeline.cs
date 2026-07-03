using Genkidama.Contracts;

namespace Genkidama.Application;

/// <summary>
/// Executes request handlers through ordered pipeline behaviors.
/// </summary>
/// <typeparam name="TRequest">The request type.</typeparam>
/// <typeparam name="TResponse">The response type.</typeparam>
public sealed class GenkidamaPipeline<TRequest, TResponse>
{
    private readonly IReadOnlyList<IGenkidamaPipelineBehavior<TRequest, TResponse>> behaviors;

    /// <summary>
    /// Initializes a new instance of the <see cref="GenkidamaPipeline{TRequest,TResponse}"/> class.
    /// </summary>
    /// <param name="behaviors">The ordered pipeline behaviors.</param>
    public GenkidamaPipeline(IEnumerable<IGenkidamaPipelineBehavior<TRequest, TResponse>> behaviors)
        => this.behaviors = behaviors.ToArray();

    /// <summary>
    /// Executes the request through all configured behaviors.
    /// </summary>
    /// <param name="request">The request.</param>
    /// <param name="handler">The terminal handler.</param>
    /// <param name="cancellationToken">The cancellation token.</param>
    /// <returns>The pipeline result.</returns>
    public Task<StandardResult<TResponse>> ExecuteAsync(
        TRequest request,
        Func<TRequest, CancellationToken, Task<StandardResult<TResponse>>> handler,
        CancellationToken cancellationToken = default)
    {
        var next = CreateTerminal(request, handler);
        foreach (var behavior in behaviors.Reverse())
        {
            next = Wrap(request, behavior, next);
        }

        return next(cancellationToken);
    }

    private static GenkidamaPipelineDelegate<TResponse> CreateTerminal(
        TRequest request,
        Func<TRequest, CancellationToken, Task<StandardResult<TResponse>>> handler)
        => token => handler(request, token);

    private static GenkidamaPipelineDelegate<TResponse> Wrap(
        TRequest request,
        IGenkidamaPipelineBehavior<TRequest, TResponse> behavior,
        GenkidamaPipelineDelegate<TResponse> next)
        => token => behavior.HandleAsync(request, next, token);
}
