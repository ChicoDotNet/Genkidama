#!/usr/bin/env python3
from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

import debt_contracts as dc

ROOT = dc.ROOT
MOJO_ROOT = ROOT / "src/Systems/Mojo"
STATE = MOJO_ROOT / "patterns.json"
MANIFEST = MOJO_ROOT / "pixi.toml"


def _string_list(data: dict[str, object], key: str) -> list[str]:
    value = data.get(key)
    dc.require(
        isinstance(value, list)
        and value
        and all(isinstance(item, str) and item for item in value),
        f"Mojo {key} census must be a non-empty string list",
    )
    result = list(value)
    dc.require(len(result) == len(set(result)), f"Mojo {key} census contains duplicates")
    return result


def load_state() -> dict[str, object]:
    try:
        data = json.loads(STATE.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise dc.ContractError(f"Unable to load Mojo pattern census: {exc}") from exc

    dc.require(data.get("schema_version") == 2, "Mojo pattern census schema_version must be 2")
    dc.require(data.get("target") == "Mojo", "Mojo pattern census target mismatch")
    dc.require(data.get("universe") == 52, "Mojo pattern universe must remain 52")

    contracted = _string_list(data, "contracted")
    implemented = _string_list(data, "implemented")
    dc.require(len(contracted) == 52, f"Mojo contracted census is {len(contracted)}; expected 52")
    dc.require(set(implemented) <= set(contracted), "Mojo implemented cells must be contracted first")
    return data


def main() -> int:
    state = load_state()
    contracted = sorted(state["contracted"])
    implemented = sorted(state["implemented"])

    expected_sources = [f"{name}.mojo" for name in implemented]
    expected_tests = [f"test_{name}.mojo" for name in contracted]

    actual_sources = sorted(
        path.name
        for path in (MOJO_ROOT / "patterns").glob("*.mojo")
        if path.name != "__init__.mojo"
    )
    actual_tests = sorted(path.name for path in (MOJO_ROOT / "tests").glob("test_*.mojo"))

    dc.require(actual_sources == expected_sources, f"Mojo canonical census mismatch: {actual_sources}")
    dc.require(actual_tests == expected_tests, f"Mojo validation census mismatch: {actual_tests}")

    pixi = ["pixi", "run", "--manifest-path", str(MANIFEST)]
    dc.run([*pixi, "mojo", "--version"])

    failed: list[str] = []
    for test_name in actual_tests:
        argv = [
            *pixi,
            "mojo",
            "run",
            "-I",
            str(MOJO_ROOT),
            str(MOJO_ROOT / "tests" / test_name),
        ]
        print(f"$ {' '.join(argv)}", flush=True)
        completed = subprocess.run(argv, cwd=ROOT, text=True, check=False)
        if completed.returncode != 0:
            failed.append(test_name)
            print(f"MOJO_CELL_FAIL {test_name} exit={completed.returncode}", flush=True)
        else:
            print(f"MOJO_CELL_PASS {test_name}", flush=True)

    aggregate_argv = [
        *pixi,
        "mojo",
        "run",
        "-I",
        str(MOJO_ROOT),
        str(MOJO_ROOT / "pattern_sweep.mojo"),
    ]
    print(f"$ {' '.join(aggregate_argv)}", flush=True)
    aggregate = subprocess.run(
        aggregate_argv,
        cwd=ROOT,
        text=True,
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )
    output = aggregate.stdout or ""
    if output:
        print(output, end="" if output.endswith("\n") else "\n", flush=True)
    expected_sentinel = f"mojo-pattern-sweep: {len(implemented)}/52 passed"
    if aggregate.returncode != 0 or dc.last_line(output) != expected_sentinel:
        failed.append("pattern_sweep.mojo")

    if failed:
        raise dc.ContractError(
            f"Mojo validation failures ({len(failed)}): {', '.join(failed)}"
        )

    print(
        f"Mojo patterns: PASS contracted={len(contracted)}/52 implemented={len(implemented)}/52",
        flush=True,
    )
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except dc.ContractError as exc:
        print(f"Mojo patterns failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
