#!/usr/bin/env python3
from __future__ import annotations

import json
import subprocess
import sys
import tempfile
from pathlib import Path

import debt_contracts as dc

ROOT = dc.ROOT
OPENCL_ROOT = ROOT / "src/Accelerated/OpenCL"
STATE = OPENCL_ROOT / "patterns.json"


def load_state() -> dict[str, object]:
    try:
        data = json.loads(STATE.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise dc.ContractError(f"Unable to load OpenCL accelerated census: {exc}") from exc
    dc.require(data.get("schema_version") == 1, "OpenCL census schema_version must be 1")
    dc.require(data.get("dimension") == "Accelerated/Heterogeneous", "OpenCL must remain outside the language denominator")
    dc.require(data.get("target") == "OpenCL", "OpenCL target mismatch")
    contracted = data.get("contracted")
    implemented = data.get("implemented")
    dc.require(isinstance(contracted, list) and len(contracted) == 12, "OpenCL pilot must contract 12 patterns")
    dc.require(len(contracted) == len(set(contracted)), "OpenCL pilot census contains duplicates")
    dc.require(isinstance(implemented, list), "OpenCL implemented census must be a list")
    dc.require(set(implemented) <= set(contracted), "OpenCL implementations must be contracted first")
    return data


def main() -> int:
    state = load_state()
    contracted = list(state["contracted"])
    implemented = list(state["implemented"])
    actual_tests = sorted(p.stem.removeprefix("test_") for p in (OPENCL_ROOT / "tests").glob("test_*.c"))
    dc.require(actual_tests == sorted(contracted), f"OpenCL test census mismatch: {actual_tests}")
    actual_sources = sorted(p.stem for p in (OPENCL_ROOT / "patterns").glob("*.c")) if (OPENCL_ROOT / "patterns").is_dir() else []
    dc.require(actual_sources == sorted(implemented), f"OpenCL implementation census mismatch: {actual_sources}")
    dc.run(["clinfo", "--list"])

    failures: list[str] = []
    with tempfile.TemporaryDirectory(prefix="genkidama-opencl-") as temp:
        work = Path(temp)
        for index, name in enumerate(contracted):
            source = OPENCL_ROOT / "patterns" / f"{name}.c"
            header = OPENCL_ROOT / "include" / f"{name}.h"
            kernel = OPENCL_ROOT / "kernels" / f"{name}.cl"
            test = OPENCL_ROOT / "tests" / f"test_{name}.c"
            missing = [p.name for p in (source, header, kernel) if not p.is_file()]
            if missing:
                failures.append(f"{name}:missing-{'+'.join(missing)}")
                print(f"OPENCL_CELL_FAIL {name} missing={','.join(missing)}", flush=True)
                continue
            binary = work / f"cell-{index}"
            argv = [
                "cc", "-std=c17", "-Wall", "-Wextra", "-Werror",
                "-I", str(OPENCL_ROOT / "include"),
                str(source), str(test), "-lOpenCL", "-o", str(binary),
            ]
            print("$ " + " ".join(argv), flush=True)
            completed = subprocess.run(argv, cwd=ROOT, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, check=False)
            output = completed.stdout or ""
            if output:
                print(output, end="" if output.endswith("\n") else "\n", flush=True)
            if completed.returncode != 0:
                failures.append(f"{name}:compile")
                print(f"OPENCL_CELL_FAIL {name} compile", flush=True)
                continue
            run = subprocess.run([str(binary), str(kernel)], cwd=ROOT, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, check=False)
            output = run.stdout or ""
            if output:
                print(output, end="" if output.endswith("\n") else "\n", flush=True)
            if run.returncode != 0:
                failures.append(f"{name}:runtime")
                print(f"OPENCL_CELL_FAIL {name} runtime", flush=True)
            else:
                print(f"OPENCL_CELL_PASS {name}", flush=True)

    if failures:
        raise dc.ContractError(f"OpenCL pilot failures ({len(failures)}): {', '.join(failures)}")
    dc.require(len(implemented) == len(contracted), f"OpenCL implemented census is {len(implemented)}; expected {len(contracted)}")
    print("OpenCL accelerated pilot: PASS contracted=12/12 implemented=12/12", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except dc.ContractError as exc:
        print(f"OpenCL accelerated pilot failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
