# Genkidama Engineering Standards

Genkidama is intended to be a modern, open source application scaffold and architecture generator. The project must remain suitable for real production work while also serving as a reference implementation for current application development practices.

## Version policy

Genkidama targets the latest stable versions of the core technologies used by generated applications.

The initial technology surface includes:

- .NET SDK and ASP.NET Core
- MSTest
- MSAL
- React
- Angular
- Bootstrap
- Fluent UI
- .NET MAUI
- WinForms
- TanStack Query
- Entity Framework Core
- Hangfire or compatible background processing providers

Each delivery must verify current stable versions before adding or upgrading generated templates. Previews and release candidates are allowed only behind an explicit experimental flag.

## Branch policy

Work starts in `genkidama-cli`.

Integration happens in `dev`.

Stable consolidation happens in `main`.

No work should be merged forward unless it builds, passes tests, and satisfies the minimum quality gates.

## Testing policy

MSTest is mandatory from the first bootstrap delivery.

The minimum global coverage gate is 44%.

The ideal global coverage target is greater than 72.8%.

Coverage must not be reduced casually. A temporary reduction requires an explicit note in the delivery documentation.

## Method design policy

Methods should be small, direct, and easy to test.

The preferred method length is ten executable lines or fewer.

A method may exceed ten lines when doing so keeps the code clearer and the cyclomatic complexity low.

Low cyclomatic complexity is more important than mechanically splitting cohesive logic.

## Documentation policy

All public and internal C# types, members, and extension points require XML documentation in English.

Generated code should include XML documentation when the generated member is public or internal.

Private implementation details may use regular comments only when the code would otherwise be unclear.

## Generated architecture policy

Generated applications should favor clear contracts, predictable runtime behavior, and simple replacement of infrastructure.

The baseline generated backend architecture includes:

- Controllers or Minimal API endpoints
- Contracts
- Services
- Repositories
- Unit of Work
- DbContext
- Domain model
- StandardResult
- StandardQuery
- StandardJob
- StandardEvent
- Notifications
- Audit trail
- Security context
- Typed API clients

## Database schema policy

Generated persistence should support schema separation for operational clarity.

The default schemas are:

- Audit
- Business
- Enum
- Security
- Utility
- Job
- Notification
- Integration

Additional schemas may be added by modules, but templates should not mix unrelated responsibilities in the same schema.

## Client policy

Generated clients should be simple enough to teach the architecture and useful enough to start real work.

The initial examples are:

- React MVC-style SPA
- Angular MVC-style SPA
- MAUI MVVM app
- WinForms MVP app
- Console runner

Each client should consume the same contract model and StandardResult-compatible API client.
