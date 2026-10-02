#!/usr/bin/env python3
from __future__ import annotations

import json
import re
import subprocess
import sys
import tempfile
from pathlib import Path

import debt_contracts as dc

ROOT = dc.ROOT
CHAPEL_ROOT = ROOT / "src/Systems/Chapel"
STATE = CHAPEL_ROOT / "patterns.json"
SWEEP = CHAPEL_ROOT / "pattern_sweep.chpl"

MODULE_RE = re.compile(r"^use\s+(Pattern[A-Za-z0-9_]+);\s*$", re.MULTILINE)
EXPECTED_RE = re.compile(r'assert\(actual\s*==\s*"([^"]*)"\);')


def load_state() -> dict[str, object]:
    try:
        data = json.loads(STATE.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise dc.ContractError(f"Unable to load Chapel pattern census: {exc}") from exc
    dc.require(data.get("schema_version") == 1, "Chapel pattern census schema_version must be 1")
    dc.require(data.get("target") == "Chapel", "Chapel target mismatch")
    dc.require(data.get("language_catalog_target") == 53, "Chapel must remain language target #53")
    dc.require(data.get("universe") == 52, "Chapel pattern universe must remain 52")
    contracted = data.get("contracted")
    implemented = data.get("implemented")
    dc.require(isinstance(contracted, list) and len(contracted) == 52, "Chapel must contract all 52 patterns")
    dc.require(len(contracted) == len(set(contracted)), "Chapel contracted census contains duplicates")
    dc.require(isinstance(implemented, list), "Chapel implemented census must be a list")
    dc.require(set(implemented) <= set(contracted), "Chapel implementations must be contracted first")
    return data


def test_contract(test: Path) -> tuple[str, str]:
    text = test.read_text(encoding="utf-8")
    module = MODULE_RE.search(text)
    expected = EXPECTED_RE.search(text)
    dc.require(module is not None, f"{test.name}: Chapel test must use exactly one Pattern module")
    dc.require(expected is not None, f"{test.name}: Chapel test must assert actual against a literal expected value")
    return module.group(1), expected.group(1)


def main() -> int:
    state = load_state()
    contracted = list(state["contracted"])
    implemented = list(state["implemented"])
    tests = CHAPEL_ROOT / "tests"

    actual_tests = sorted(p.stem.removeprefix("test_") for p in tests.glob("test_*.chpl"))
    dc.require(actual_tests == sorted(contracted), f"Chapel test census mismatch: {actual_tests}")
    actual_sources = sorted(p.stem for p in (CHAPEL_ROOT / "patterns").glob("*.chpl"))
    dc.require(actual_sources == sorted(implemented), f"Chapel implementation census mismatch: {actual_sources}")
    dc.require(len(implemented) == 52, f"Chapel implemented census is {len(implemented)}; expected 52")
    dc.require(SWEEP.is_file(), "Chapel aggregate pattern_sweep.chpl is missing")

    expected: dict[str, str] = {}
    expected_modules: dict[str, str] = {}
    for name in contracted:
        module, value = test_contract(tests / f"test_{name}.chpl")
        expected[name] = value
        expected_modules[name] = module

    sweep_text = SWEEP.read_text(encoding="utf-8")
    for name in contracted:
        module = expected_modules[name]
        dc.require(
            f"import {module};" in sweep_text,
            f"Chapel aggregate runner does not import {module} for {name}",
        )
        dc.require(
            f'{module}.run()' in sweep_text,
            f"Chapel aggregate runner does not execute {name}",
        )

    dc.run(["chpl", "--version"])
    sources = [str(CHAPEL_ROOT / "patterns" / f"{name}.chpl") for name in contracted]

    with tempfile.TemporaryDirectory(prefix="genkidama-chapel-") as temp:
        binary = Path(temp) / "pattern-sweep"
        argv = ["chpl", "--warnings", *sources, str(SWEEP), "-o", str(binary)]
        print("$ " + " ".join(argv), flush=True)
        compiled = subprocess.run(
            argv,
            cwd=ROOT,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            check=False,
        )
        compile_output = compiled.stdout or ""
        if compile_output:
            print(compile_output, end="" if compile_output.endswith("\n") else "\n", flush=True)
        dc.require(compiled.returncode == 0, f"Chapel aggregate compile failed exit={compiled.returncode}")

        run = subprocess.run(
            [str(binary)],
            cwd=ROOT,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            check=False,
        )
        output = run.stdout or ""
        if output:
            print(output, end="" if output.endswith("\n") else "\n", flush=True)
        dc.require(run.returncode == 0, f"Chapel aggregate runtime failed exit={run.returncode}")

    observed: dict[str, str] = {}
    sentinel = None
    for raw in output.splitlines():
        if "\t" not in raw:
            continue
        name, value = raw.split("\t", 1)
        if name == "__CHAPEL_SWEEP_DONE__":
            sentinel = value
        else:
            dc.require(name not in observed, f"Chapel aggregate emitted duplicate cell {name}")
            observed[name] = value

    failures: list[str] = []
    expected_names = set(contracted)
    observed_names = set(observed)
    for missing in sorted(expected_names - observed_names):
        failures.append(f"{missing}:missing-output")
        print(f"CHAPEL_CELL_FAIL {missing} missing-output", flush=True)
    for extra in sorted(observed_names - expected_names):
        failures.append(f"{extra}:unexpected-output")
        print(f"CHAPEL_CELL_FAIL {extra} unexpected-output", flush=True)

    for name in contracted:
        if name not in observed:
            continue
        if observed[name] == expected[name]:
            print(f"CHAPEL_CELL_PASS {name}", flush=True)
        else:
            failures.append(f"{name}:expected={expected[name]!r}:actual={observed[name]!r}")
            print(
                f"CHAPEL_CELL_FAIL {name} expected={expected[name]!r} actual={observed[name]!r}",
                flush=True,
            )

    dc.require(sentinel == "52", f"Chapel aggregate completion sentinel mismatch: {sentinel!r}")
    if failures:
        raise dc.ContractError(f"Chapel validation failures ({len(failures)}): {', '.join(failures)}")

    print("chapel-pattern-sweep: 52/52 passed", flush=True)
    print("Chapel patterns: PASS contracted=52/52 implemented=52/52", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except dc.ContractError as exc:
        print(f"Chapel patterns failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
