# Architecture Reference

This section captures the architectural rules that should guide Genkidama blueprints, CLI generators and generated applications.

## Current references

- `solution-composition.md`: solution membership, project references and local developer experience rules.
- `decisions/`: decision records for architecture choices.

## Maintenance rules

- Architecture references should explain the intent behind project layout decisions.
- Any project referenced by another project should be included in `Genkidama.slnx` unless there is a documented reason not to include it.
- CI compatibility is necessary but not sufficient; Visual Studio and command line restore should both be considered.
- Important architectural choices should be recorded in `decisions/`.
