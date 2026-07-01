# Contributing to Genkidama

Thank you for helping improve Genkidama.

Genkidama is an open source .NET CLI and application scaffold for modern, contract-first applications. It also includes an educational Design Patterns Example App.

## Branch flow

Use this flow for project work:

```text
genkidama-cli -> dev -> main
```

- `genkidama-cli`: active feature work.
- `dev`: integration branch.
- `main`: stable public branch.

## Delivery flow

Each meaningful change should map to a GEN delivery when possible.

Examples:

- `GEN-000`: repository preparation.
- `GEN-001`: CLI bootstrap.
- `GEN-003`: StandardResult and StandardQuery.

## Definition of Done

A change is complete when:

- It builds successfully.
- MSTest passes.
- Global coverage stays at or above 44%.
- XML documentation is updated for public and internal C# APIs.
- Engineering standards are respected.
- The roadmap is updated when scope changes.
- New user-facing text uses localization keys when applicable.

## Code style

Prefer small methods.

The preferred method length is ten executable lines or fewer.

A method may be longer when that keeps the code clearer and cyclomatic complexity low.

Use descriptive names and keep the generated architecture easy to read.

## Documentation style

All public and internal C# types, members and extension points require XML documentation in English.

Educational documentation may be localized.

Generated source code, namespaces, public members and XML documentation remain in English.

## Testing

MSTest is mandatory.

The minimum global coverage gate is 44%.

The ideal target is greater than 72.8%.

Do not reduce coverage casually.

## Localization

Genkidama uses stable localization keys through contract constants and .resx resources where appropriate.

Supported educational locales are:

- English
- Spanish
- French
- German
- Italian
- Portuguese
- Japanese
- Simplified Chinese

## Design Patterns education

The Design Patterns Example App should show patterns as practical, living examples.

Do not force a pattern where a simpler solution is clearer.

The architecture comes first. The pattern catalog supports the architecture.
