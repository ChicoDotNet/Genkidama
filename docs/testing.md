# Testing Guidance

## Goals

Genkidama tests should be deterministic, isolated and fast enough to run on every pull request.

## Rules

- Use MSTest for .NET tests.
- Prefer pure unit tests over integration tests unless persistence, HTTP or CLI boundaries are being verified.
- Use isolated temporary directories for file generation tests.
- Do not depend on the current user's home directory.
- Do not depend on wall clock time unless the test controls the value.
- Keep generated output assertions specific and readable.
- Keep test names in the `Method_WithCondition_ExpectedResult` style.

## CI expectations

The CI workflow restores the solution, builds it in Release mode and runs tests with code coverage. Test results are written to a stable `TestResults` directory.

## Workspace tests

When a test writes files, use a disposable workspace helper so the test can clean up after itself and avoid collisions with other test runs.
