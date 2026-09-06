#!/usr/bin/env python3
from __future__ import annotations

import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
EXPECTED = 39


class ContractError(RuntimeError):
    pass


def run(argv: list[str], *, cwd: Path = ROOT, capture: bool = False) -> str:
    print(f"$ {' '.join(argv)}", flush=True)
    completed = subprocess.run(
        argv,
        cwd=cwd,
        text=True,
        check=False,
        stdout=subprocess.PIPE if capture else None,
        stderr=subprocess.STDOUT if capture else None,
    )
    if completed.returncode != 0:
        if capture and completed.stdout:
            print(completed.stdout, file=sys.stderr)
        raise ContractError(f"command failed with exit {completed.returncode}: {' '.join(argv)}")
    if capture and completed.stdout:
        print(completed.stdout, end="" if completed.stdout.endswith("\n") else "\n", flush=True)
    return completed.stdout or ""


def exact_files(path: Path, suffix: str, label: str) -> list[Path]:
    files = sorted(path.glob(f"*{suffix}"))
    if len(files) != EXPECTED:
        raise ContractError(f"{label} pattern cell count is {len(files)}; expected {EXPECTED}")
    return files


def canonical_pattern_ids(files: list[Path], label: str) -> set[str]:
    identifiers = [path.stem.replace("-", "_") for path in files]
    unique = set(identifiers)
    if len(unique) != EXPECTED:
        raise ContractError(
            f"{label} canonical pattern ids contain collisions: {len(unique)} unique; expected {EXPECTED}"
        )
    return unique


def main() -> int:
    r_files = exact_files(ROOT / "src/DataScience/R/patterns", ".R", "R")
    octave_files = exact_files(ROOT / "src/DataScience/Octave/patterns", ".m", "GNU Octave")
    powershell_files = exact_files(ROOT / "src/Scripting/PowerShell/patterns", ".ps1", "PowerShell")

    pattern_ids = {
        "R": canonical_pattern_ids(r_files, "R"),
        "GNU Octave": canonical_pattern_ids(octave_files, "GNU Octave"),
        "PowerShell": canonical_pattern_ids(powershell_files, "PowerShell"),
    }
    canonical = pattern_ids["R"]
    for label, identifiers in pattern_ids.items():
        if identifiers != canonical:
            missing = sorted(canonical - identifiers)
            extra = sorted(identifiers - canonical)
            raise ContractError(
                f"Data/Shell canonical pattern inventory mismatch for {label}: "
                f"missing={missing} extra={extra}"
            )

    run(["R", "--version"], capture=True)
    run(["octave", "--version"], capture=True)
    run(
        [
            "pwsh",
            "-NoLogo",
            "-NoProfile",
            "-Command",
            "$PSVersionTable.PSVersion.ToString()",
        ],
        capture=True,
    )

    for source in r_files:
        run(["Rscript", str(source)])
        print(f"PASS R {source.name}", flush=True)

    octave_dir = ROOT / "src/DataScience/Octave/patterns"
    octave_path = octave_dir.as_posix().replace("'", "''")
    for source in octave_files:
        expression = f"addpath('{octave_path}'); {source.stem}"
        run(["octave", "--no-gui", "--quiet", "--eval", expression])
        print(f"PASS GNU Octave {source.name}", flush=True)

    for source in powershell_files:
        run(["pwsh", "-NoLogo", "-NoProfile", "-File", str(source)])
        print(f"PASS PowerShell {source.name}", flush=True)

    print(f"R pattern cells: {EXPECTED}/{EXPECTED} passed", flush=True)
    print(f"GNU Octave pattern cells: {EXPECTED}/{EXPECTED} passed", flush=True)
    print(f"PowerShell pattern cells: {EXPECTED}/{EXPECTED} passed", flush=True)
    print(f"Data/Shell Patterns contract: PASS validations={EXPECTED * 3}", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ContractError as exc:
        print(f"Data/Shell Patterns contract failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
