#!/usr/bin/env python3
from __future__ import annotations

import json
import subprocess
import sys
import tempfile
from pathlib import Path

import debt_contracts as dc

ROOT = dc.ROOT
CHAPEL_ROOT = ROOT / "src/Systems/Chapel"
STATE = CHAPEL_ROOT / "patterns.json"


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


def main() -> int:
    state = load_state()
    contracted = list(state["contracted"])
    implemented = list(state["implemented"])
    tests = CHAPEL_ROOT / "tests"
    actual_tests = sorted(p.stem.removeprefix("test_") for p in tests.glob("test_*.chpl"))
    dc.require(actual_tests == sorted(contracted), f"Chapel test census mismatch: {actual_tests}")
    actual_sources = sorted(p.stem for p in (CHAPEL_ROOT / "patterns").glob("*.chpl")) if (CHAPEL_ROOT / "patterns").is_dir() else []
    dc.require(actual_sources == sorted(implemented), f"Chapel implementation census mismatch: {actual_sources}")

    dc.run(["chpl", "--version"])
    failures: list[str] = []
    with tempfile.TemporaryDirectory(prefix="genkidama-chapel-") as temp:
        work = Path(temp)
        for index, name in enumerate(contracted):
            source = CHAPEL_ROOT / "patterns" / f"{name}.chpl"
            test = CHAPEL_ROOT / "tests" / f"test_{name}.chpl"
            if not source.is_file():
                failures.append(f"{name}:missing-source")
                print(f"CHAPEL_CELL_FAIL {name} missing-source", flush=True)
                continue
            binary = work / f"cell-{index}"
            argv = ["chpl", "--warnings", str(source), str(test), "-o", str(binary)]
            print("$ " + " ".join(argv), flush=True)
            completed = subprocess.run(argv, cwd=ROOT, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, check=False)
            output = completed.stdout or ""
            if output:
                print(output, end="" if output.endswith("\n") else "\n", flush=True)
            if completed.returncode != 0:
                failures.append(f"{name}:compile")
                print(f"CHAPEL_CELL_FAIL {name} compile", flush=True)
                continue
            run = subprocess.run([str(binary)], cwd=ROOT, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, check=False)
            output = run.stdout or ""
            if output:
                print(output, end="" if output.endswith("\n") else "\n", flush=True)
            if run.returncode != 0:
                failures.append(f"{name}:runtime")
                print(f"CHAPEL_CELL_FAIL {name} runtime", flush=True)
            else:
                print(f"CHAPEL_CELL_PASS {name}", flush=True)

    if failures:
        raise dc.ContractError(f"Chapel validation failures ({len(failures)}): {', '.join(failures)}")
    dc.require(len(implemented) == 52, f"Chapel implemented census is {len(implemented)}; expected 52")
    print("Chapel patterns: PASS contracted=52/52 implemented=52/52", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except dc.ContractError as exc:
        print(f"Chapel patterns failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
