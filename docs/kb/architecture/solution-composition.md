# Solution Composition

Generated and repository projects should be easy to restore, build and inspect from both command line tooling and Visual Studio.

## Rule

If a project is referenced by another project that belongs to the solution, the referenced project should also be listed in `Genkidama.slnx`.

## Why this matters

CI can sometimes restore transitively through a project reference even when the referenced project is not listed in the solution file. Local IDE tooling may behave differently. Visual Studio can report restore errors when a referenced project is missing from the loaded solution, even if command line CI is green.

## Example

`Genkidama.Cli.Tests` references `Genkidama.Blueprints`. Therefore `Genkidama.Blueprints` must also be included in the `/src/` folder of `Genkidama.slnx`.

## Checklist

- New source projects are added to `Genkidama.slnx`.
- New test projects are added to `Genkidama.slnx`.
- Project references do not point to projects hidden from the solution.
- Workarounds used for connector limitations are revisited before closing the next Knowledge Base or architecture delivery.

## Local validation

After pulling a change that adds project references, run restore from the command line and reload the solution in Visual Studio. Both should agree on project membership.
