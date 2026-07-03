# Genkidama Console Reference Client

This reference client demonstrates how a .NET console application can consume Genkidama APIs that return standard result and collection shapes.

The client is intentionally small:

- .NET console application.
- `HttpClient` based.
- `System.Text.Json` / `System.Net.Http.Json` based.
- No external packages.
- Typed backend client included.

## Files

- `Genkidama.ConsoleReference/ApiResult.cs` defines a deserializable result model.
- `Genkidama.ConsoleReference/ApiCollection.cs` defines a deserializable collection model.
- `Genkidama.ConsoleReference/ConsoleBackendClient.cs` wraps typed backend calls.
- `Genkidama.ConsoleReference/Program.cs` shows reference usage.

## Usage

```bash
dotnet run --project clients/console-reference/Genkidama.ConsoleReference -- http://localhost:5000/
```

This folder is a reference implementation. Future template deliveries can copy or generate these files into new console applications.
