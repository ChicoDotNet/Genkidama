# Genkidama React Reference Client

This reference client demonstrates how a React application can consume Genkidama APIs that return `StandardResult<TValue>` and `StandardCollectionResult<TItem>` shapes.

The client is intentionally small and framework-light:

- TypeScript-first.
- Fetch API only.
- No generated runtime dependency.
- No state management package requirement.
- Compatible with React hooks.

## Files

- `src/standardResult.ts` defines shared response contracts.
- `src/standardApiClient.ts` wraps HTTP calls and maps Genkidama standard results.
- `src/useStandardResultQuery.tsx` provides a small React hook for read operations.
- `src/ExampleDashboard.tsx` shows a reference usage pattern.

## Direction

This folder is a reference implementation, not a published package. Future template deliveries can copy or generate these files into new applications.
