#!/usr/bin/env python3
from __future__ import annotations

import json
import sys
from pathlib import Path

import debt_contracts as dc

ROOT = dc.ROOT
MOJO_ROOT = ROOT / "src/Systems/Mojo"
STATE = MOJO_ROOT / "patterns.json"
MANIFEST = MOJO_ROOT / "pixi.toml"


def load_state() -> dict[str, object]:
    try:
        data = json.loads(STATE.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise dc.ContractError(f"Unable to load Mojo pattern census: {exc}") from exc
    dc.require(data.get("schema_version") == 1, "Mojo pattern census schema_version must be 1")
    dc.require(data.get("target") == "Mojo", "Mojo pattern census target mismatch")
    dc.require(data.get("universe") == 52, "Mojo pattern universe must remain 52")
    implemented = data.get("implemented")
    dc.require(
        isinstance(implemented, list)
        and implemented
        and all(isinstance(item, str) and item for item in implemented),
        "Mojo implemented census must be a non-empty string list",
    )
    dc.require(len(implemented) == len(set(implemented)), "Mojo implemented census contains duplicates")
    return data


def main() -> int:
    state = load_state()
    implemented = sorted(state["implemented"])
    expected_sources = [f"{name}.mojo" for name in implemented]
    expected_tests = [f"test_{name}.mojo" for name in implemented]

    actual_sources = sorted(
        path.name
        for path in (MOJO_ROOT / "patterns").glob("*.mojo")
        if path.name != "__init__.mojo"
    )
    actual_tests = sorted(path.name for path in (MOJO_ROOT / "tests").glob("test_*.mojo"))

    dc.require(actual_sources == expected_sources, f"Mojo canonical census mismatch: {actual_sources}")
    dc.require(actual_tests == expected_tests, f"Mojo test census mismatch: {actual_tests}")

    pixi = ["pixi", "run", "--manifest-path", str(MANIFEST)]
    dc.run([*pixi, "mojo", "--version"])

    for test_name in actual_tests:
        dc.run(
            [
                *pixi,
                "mojo",
                "run",
                "-I",
                str(MOJO_ROOT),
                str(MOJO_ROOT / "tests" / test_name),
            ]
        )

    output = dc.run(
        [
            *pixi,
            "mojo",
            "run",
            "-I",
            str(MOJO_ROOT),
            str(MOJO_ROOT / "pattern_sweep.mojo"),
        ],
        capture=True,
    )
    expected_sentinel = f"mojo-pattern-sweep: {len(implemented)}/52 calibration passed"
    dc.require(dc.last_line(output) == expected_sentinel, "Mojo calibration aggregate output mismatch")
    print(f"Mojo patterns: PASS cells={len(implemented)}/52", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except dc.ContractError as exc:
        print(f"Mojo patterns failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
