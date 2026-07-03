# Genkidama Angular Reference Client

This reference client demonstrates how an Angular application can consume Genkidama APIs that return `StandardResult<TValue>` and `StandardCollectionResult<TItem>` shapes.

The client is intentionally small:

- TypeScript-first.
- Angular `HttpClient` based.
- No generated runtime dependency.
- No package publishing decision.
- Compatible with standalone components.

## Files

- `src/standard-result.ts` defines shared response contracts.
- `src/standard-api-client.ts` wraps HTTP calls and maps Genkidama standard results.
- `src/standard-result-query.ts` exposes a small query state helper.
- `src/example-dashboard.component.ts` shows a reference usage pattern.

## Direction

This folder is a reference implementation, not a published package. Future template deliveries can copy or generate these files into new Angular applications.
