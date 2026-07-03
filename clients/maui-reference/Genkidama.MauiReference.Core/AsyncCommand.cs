using System.Windows.Input;

namespace Genkidama.MauiReference;

/// <summary>
/// Provides an async command implementation without external MVVM packages.
/// </summary>
public sealed class AsyncCommand : ICommand
{
    private readonly Func<Task> executeAsync;
    private readonly Func<bool>? canExecute;
    private bool isRunning;

    /// <summary>
    /// Initializes a new instance of the <see cref="AsyncCommand"/> class.
    /// </summary>
    public AsyncCommand(Func<Task> executeAsync, Func<bool>? canExecute = null)
    {
        this.executeAsync = executeAsync;
        this.canExecute = canExecute;
    }

    /// <inheritdoc />
    public event EventHandler? CanExecuteChanged;

    /// <inheritdoc />
    public bool CanExecute(object? parameter)
        => !isRunning && (canExecute?.Invoke() ?? true);

    /// <inheritdoc />
    public async void Execute(object? parameter)
    {
        if (!CanExecute(parameter)) return;
        await RunAsync();
    }

    /// <summary>
    /// Executes the command as a task for tests.
    /// </summary>
    public async Task RunAsync()
    {
        isRunning = true;
        RaiseCanExecuteChanged();
        try
        {
            await executeAsync();
        }
        finally
        {
            isRunning = false;
            RaiseCanExecuteChanged();
        }
    }

    /// <summary>
    /// Raises the can execute changed event.
    /// </summary>
    public void RaiseCanExecuteChanged()
        => CanExecuteChanged?.Invoke(this, EventArgs.Empty);
}
