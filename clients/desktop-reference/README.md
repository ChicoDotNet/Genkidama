# Genkidama Desktop Reference Client

This reference client demonstrates a Windows Forms MVP client for Genkidama APIs.

The client is intentionally small:

- Windows Forms.
- MVP separation.
- `HttpClient` based backend access.
- `System.Text.Json` / `System.Net.Http.Json` based responses.
- No external runtime packages.

## Files

- `Genkidama.DesktopReference/PrimaryForm.cs` provides the desktop view.
- `Genkidama.DesktopReference/IPrimaryView.cs` defines the view contract.
- `Genkidama.DesktopReference/PrimaryPresenter.cs` contains testable view coordination logic.
- `Genkidama.DesktopReference/DesktopBackendClient.cs` wraps backend calls.

## Usage

```bash
dotnet run --project clients/desktop-reference/Genkidama.DesktopReference
```

This folder is a reference implementation. Future template deliveries can copy or generate these files into new desktop applications.
