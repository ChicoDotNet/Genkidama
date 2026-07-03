# Genkidama MAUI MVVM Reference Client

This reference client demonstrates a .NET MAUI MVVM client for Genkidama APIs.

The delivery is split into two parts:

- `Genkidama.MauiReference.Core`: portable `net10.0` MVVM logic that CI can build and test without MAUI workloads.
- `source-only`: MAUI XAML and bootstrap files that can be copied into a real MAUI application template.

## Core files

- `ObservableObject.cs` provides `INotifyPropertyChanged` support.
- `AsyncCommand.cs` provides a minimal async `ICommand` implementation.
- `MauiBackendClient.cs` wraps backend calls with `HttpClient`.
- `MainViewModel.cs` exposes bindable MVVM state and refresh behavior.

## Source-only MAUI files

- `App.xaml` and `App.xaml.cs`.
- `MainPage.xaml` and `MainPage.xaml.cs`.
- `MauiProgram.cs`.

## Direction

This folder is a reference implementation. The core is compiled and tested today. The MAUI app shell is intentionally source-only until the repository adds a dedicated MAUI workload build job or template generation stage.
