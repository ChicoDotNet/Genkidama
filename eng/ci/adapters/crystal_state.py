#!/usr/bin/env python3
from __future__ import annotations

import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
SOURCE = ROOT / "src/Niche/Crystal/state.cr"
EXPECTED = "crystal-state: passed"


def run(argv: list[str], *, capture: bool = False) -> str:
    print(f"$ {' '.join(argv)}", flush=True)
    completed = subprocess.run(
        argv,
        cwd=ROOT,
        text=True,
        check=False,
        stdout=subprocess.PIPE if capture else None,
        stderr=subprocess.STDOUT if capture else None,
    )
    output = completed.stdout or ""
    if capture and output:
        print(output, end="" if output.endswith("\n") else "\n", flush=True)
    if completed.returncode != 0:
        raise SystemExit(completed.returncode)
    return output


def main() -> None:
    if not SOURCE.is_file():
        raise SystemExit(f"missing Crystal State canonical: {SOURCE.relative_to(ROOT)}")

    run(["crystal", "tool", "format", "--check", str(SOURCE)])
    with tempfile.TemporaryDirectory(prefix="genkidama-crystal-state-") as temp:
        binary = Path(temp) / "crystal-state"
        run(["crystal", "build", "--error-on-warnings", str(SOURCE), "-o", str(binary)])
        lines = [line.strip() for line in run([str(binary)], capture=True).splitlines() if line.strip()]
        if not lines or lines[-1] != EXPECTED:
            raise SystemExit(f"Crystal State output mismatch: expected {EXPECTED!r}")


if __name__ == "__main__":
    main()
