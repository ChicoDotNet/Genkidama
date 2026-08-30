#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import json
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parents[2]
LEDGER = ROOT / "eng/ci/workflow_debt.json"
WORKFLOW_DIR = ROOT / ".github/workflows"
FROZEN_BASELINE_SHA256 = "97e3b0e563f2c33338430797505925dba1519bbd3e7a89899c95e4d3b7b9be69"
EXPECTED_RETAINED = {"ci.yml", "polyglot.yml", "quality.yml"}
ALLOWED_STATES = {"present", "retired"}
ALLOWED_DISPOSITIONS = {"extracted", "superseded", "unresolved"}


class WorkflowDebtError(RuntimeError):
    pass


@dataclass(frozen=True)
class Stats:
    retained: int
    legacy: int
    present: int
    retired: int
    extracted: int
    superseded: int
    unresolved: int


def baseline_digest(names: list[str]) -> str:
    payload = "".join(f"{name}\n" for name in sorted(names))
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()


def _load(path: Path) -> dict[str, Any]:
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except FileNotFoundError as exc:
        raise WorkflowDebtError(f"workflow debt ledger missing: {path}") from exc
    except json.JSONDecodeError as exc:
        raise WorkflowDebtError(f"workflow debt ledger is invalid JSON: {exc}") from exc
    if not isinstance(data, dict):
        raise WorkflowDebtError("workflow debt ledger root must be an object")
    return data


def _actual_workflows(root: Path) -> set[str]:
    directory = root / ".github/workflows"
    if not directory.is_dir():
        raise WorkflowDebtError(f"workflow directory missing: {directory}")
    return {
        path.name
        for path in directory.iterdir()
        if path.is_file() and path.suffix.lower() in {".yml", ".yaml"}
    }


def validate(
    *,
    root: Path = ROOT,
    ledger_path: Path | None = None,
    require_retirable: bool = False,
) -> Stats:
    ledger_path = ledger_path or (root / "eng/ci/workflow_debt.json")
    data = _load(ledger_path)

    if data.get("schema_version") != 1:
        raise WorkflowDebtError(f"unsupported workflow debt schema: {data.get('schema_version')!r}")

    retained = data.get("retained")
    if not isinstance(retained, list) or any(not isinstance(item, str) or not item for item in retained):
        raise WorkflowDebtError("retained must be a non-empty list of workflow filenames")
    retained_set = set(retained)
    if len(retained_set) != len(retained):
        raise WorkflowDebtError("retained contains duplicate workflow filenames")
    if retained_set != EXPECTED_RETAINED:
        raise WorkflowDebtError(
            f"retained clean-slate workflow set changed: expected={sorted(EXPECTED_RETAINED)} actual={sorted(retained_set)}"
        )

    baseline = data.get("baseline")
    if not isinstance(baseline, dict):
        raise WorkflowDebtError("baseline must be an object")
    names = baseline.get("legacy_names")
    declared_digest = baseline.get("sha256")
    if not isinstance(names, list) or any(not isinstance(item, str) or not item for item in names):
        raise WorkflowDebtError("baseline.legacy_names must be a list of workflow filenames")
    if len(set(names)) != len(names):
        raise WorkflowDebtError("baseline.legacy_names contains duplicates")
    actual_digest = baseline_digest(names)
    if declared_digest != actual_digest:
        raise WorkflowDebtError(
            f"baseline digest mismatch: declared={declared_digest!r} computed={actual_digest}"
        )
    if actual_digest != FROZEN_BASELINE_SHA256:
        raise WorkflowDebtError(
            f"frozen I10 baseline changed: expected={FROZEN_BASELINE_SHA256} actual={actual_digest}"
        )

    records = data.get("workflows")
    if not isinstance(records, dict):
        raise WorkflowDebtError("workflows must be an object")
    if set(records) != set(names):
        missing = sorted(set(names) - set(records))
        extra = sorted(set(records) - set(names))
        raise WorkflowDebtError(f"workflow ledger keys differ from frozen baseline: missing={missing} extra={extra}")

    actual = _actual_workflows(root)
    unknown = sorted(actual - retained_set - set(names))
    if unknown:
        raise WorkflowDebtError(f"uncensused workflow files detected: {unknown}")
    missing_retained = sorted(retained_set - actual)
    if missing_retained:
        raise WorkflowDebtError(f"retained clean-slate workflows are missing: {missing_retained}")

    counts = {key: 0 for key in ALLOWED_DISPOSITIONS}
    present = 0
    retired = 0
    unresolved_names: list[str] = []

    for name in sorted(names):
        record = records[name]
        if not isinstance(record, dict):
            raise WorkflowDebtError(f"{name}: record must be an object")

        state = record.get("state")
        disposition = record.get("disposition")
        owner = record.get("owner")
        evidence = record.get("evidence", [])
        reason = record.get("reason")

        if state not in ALLOWED_STATES:
            raise WorkflowDebtError(f"{name}: invalid state {state!r}")
        if disposition not in ALLOWED_DISPOSITIONS:
            raise WorkflowDebtError(f"{name}: invalid disposition {disposition!r}")
        if not isinstance(owner, str) or not owner.strip():
            raise WorkflowDebtError(f"{name}: owner is required")
        if not isinstance(evidence, list) or any(not isinstance(item, str) or not item for item in evidence):
            raise WorkflowDebtError(f"{name}: evidence must be a list of repository-relative paths")

        exists = name in actual
        if state == "present":
            present += 1
            if not exists:
                raise WorkflowDebtError(f"{name}: ledger says present but workflow file is absent")
        else:
            retired += 1
            if exists:
                raise WorkflowDebtError(f"{name}: ledger says retired but workflow file still exists")
            if disposition == "unresolved":
                raise WorkflowDebtError(f"{name}: unresolved workflow cannot be retired")

        if disposition in {"extracted", "superseded"}:
            if not evidence:
                raise WorkflowDebtError(f"{name}: {disposition} workflow requires evidence")
            for relative in evidence:
                path = Path(relative)
                if path.is_absolute() or ".." in path.parts:
                    raise WorkflowDebtError(f"{name}: invalid evidence path {relative!r}")
                if path.parts[:2] == (".github", "workflows"):
                    raise WorkflowDebtError(f"{name}: evidence must live outside workflow YAML: {relative}")
                if not (root / path).exists():
                    raise WorkflowDebtError(f"{name}: evidence path does not exist: {relative}")
        else:
            unresolved_names.append(name)
            if not isinstance(reason, str) or not reason.strip():
                raise WorkflowDebtError(f"{name}: unresolved workflow requires a reason")

        counts[disposition] += 1

    if require_retirable and unresolved_names:
        raise WorkflowDebtError(
            "legacy workflow purge is not yet safe; unresolved contracts: "
            + ", ".join(unresolved_names)
        )

    stats = Stats(
        retained=len(retained_set),
        legacy=len(names),
        present=present,
        retired=retired,
        extracted=counts["extracted"],
        superseded=counts["superseded"],
        unresolved=counts["unresolved"],
    )
    print(
        "WORKFLOW_DEBT "
        f"retained={stats.retained} legacy={stats.legacy} present={stats.present} retired={stats.retired} "
        f"extracted={stats.extracted} superseded={stats.superseded} unresolved={stats.unresolved}",
        flush=True,
    )
    if unresolved_names:
        print("WORKFLOW_DEBT unresolved=" + ",".join(unresolved_names), flush=True)
    return stats


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Fail-closed census for legacy GitHub Actions workflow debt.")
    subparsers = parser.add_subparsers(dest="command", required=True)
    validate_parser = subparsers.add_parser("validate")
    validate_parser.add_argument(
        "--require-retirable",
        action="store_true",
        help="Fail while any legacy workflow still has unresolved contract debt.",
    )
    args = parser.parse_args(argv)
    try:
        validate(require_retirable=args.require_retirable)
    except WorkflowDebtError as exc:
        print(f"Workflow debt validation failed: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
