#!/usr/bin/env python3
from __future__ import annotations

import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
WORKFLOW_DIR = ROOT / ".github" / "workflows"
# Permanent fail-closed guardrail for the post-I10 workflow topology.
ALLOWED_WORKFLOWS = frozenset({"ci.yml", "quality.yml", "polyglot.yml"})


class WorkflowInventoryError(RuntimeError):
    """Raised when the consolidated workflow inventory drifts."""


def actual_workflows() -> set[str]:
    return {
        path.relative_to(WORKFLOW_DIR).as_posix()
        for path in WORKFLOW_DIR.rglob("*")
        if path.is_file()
    }


def summary() -> dict[str, object]:
    actual = actual_workflows()
    return {
        "allowed_workflow_count": len(ALLOWED_WORKFLOWS),
        "workflow_count": len(actual),
        "workflows": sorted(actual),
        "unexpected": sorted(actual - ALLOWED_WORKFLOWS),
        "missing": sorted(ALLOWED_WORKFLOWS - actual),
    }


def validate() -> dict[str, object]:
    stats = summary()
    unexpected = stats["unexpected"]
    missing = stats["missing"]
    if unexpected or missing:
        raise WorkflowInventoryError(
            f"workflow inventory drift: unexpected={unexpected}, missing={missing}"
        )
    return {**stats, "status": "consolidated"}


def main(argv: list[str] | None = None) -> int:
    args = list(sys.argv[1:] if argv is None else argv)
    command = args[0] if args else "validate"
    if len(args) > 1 or command not in {"summary", "validate"}:
        raise WorkflowInventoryError("usage: workflow_debt.py [summary|validate]")

    payload = validate() if command == "validate" else summary()
    print(json.dumps(payload, sort_keys=True))
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except WorkflowInventoryError as exc:
        print(f"workflow-inventory: {exc}", file=sys.stderr)
        raise SystemExit(1)
