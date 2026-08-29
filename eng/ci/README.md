# Genkidama local CI engine

`eng/ci` is the executable contract beneath GitHub Actions. Workflows are expected to become thin launchers over these commands; test behavior must live here or in test projects, not inline in YAML.

## Stable entry points

On POSIX shells:

```bash
eng/ci/run-product
eng/ci/run-quality
eng/ci/run-family dotnet --patterns
eng/ci/run-family jvm --patterns
eng/ci/run-family native --patterns
eng/ci/run-family platform --patterns
eng/ci/run-family portable-functional --patterns
eng/ci/detect-changes BASE HEAD
```

On Windows, invoke the same extensionless Python entry points through the Python launcher, for example `py eng\\ci\\run-product`.

Every runner follows the same process contract:

- exit code `0` means certified;
- any non-zero exit code means failed;
- commands are printed before execution;
- `setup`, `validation`, and `total` wall-clock seconds are emitted as one `CI_TELEMETRY` JSON record;
- `--telemetry-file <path>` appends the same record as JSON Lines;
- `--dry-run` prints the planned commands without executing toolchains.

The runner is fail-closed: after a failed command, later commands in that target are not executed.

## Targets introduced in I1

- `run-product` reproduces the product contract from the historical `ci.yml`: restore, Release build, tests with coverage, and CLI pack.
- `run-quality` tests this engine and executes the existing Learn metadata/structure/link validator.
- `run-family portable-functional --patterns` delegates to the already-certified 507-cell cohort validator.
- `dotnet`, `jvm`, and `native` extract the behavioral Prototype assertion into a local adapter for C#, Java, and Rust respectively.
- `platform` extracts the existing VBA and Delphi Abstract Factory source contracts from workflow YAML into a local test adapter.

The representative `dotnet`, `jvm`, `native`, and `platform` adapters are architectural probes for I1. They do **not** claim full language-family coverage yet. Until later increments expand each family, `detect-changes` only selects them for the exact contracts they currently cover. Unknown paths deliberately fall back to `full=true`.

## Change detection

`detect-changes BASE HEAD` reads `git diff --name-only --diff-filter=ACMR BASE HEAD` and emits one JSON object:

```json
{
  "product": true,
  "quality": false,
  "polyglot": ["native"],
  "learn_languages": [],
  "full": false,
  "unknown_paths": []
}
```

For tests and debugging, paths can be supplied directly:

```bash
eng/ci/detect-changes --path src/Systems/Rust/prototype.rs
eng/ci/detect-changes --full
```

`eng/ci/**` changes and any path not understood by the current registry force a full run. This is intentionally conservative while the old workflow mesh is still present.

## Registry

`registry.json` is data only. It declares:

- executable targets and their `setup` / `validation` commands;
- family-to-surface mappings;
- path-to-gate/family mappings for change detection.

No workflow is migrated or deleted in I1. The 84 existing workflow files remain untouched so equivalence can be established incrementally in later stages.
