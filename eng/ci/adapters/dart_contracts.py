#!/usr/bin/env python3
from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]


class ContractError(RuntimeError):
    pass


def _command(argv: list[str]) -> list[str]:
    if os.name != "nt":
        return argv
    executable = shutil.which(argv[0])
    if executable is None:
        for suffix in (".bat", ".cmd", ".exe"):
            executable = shutil.which(argv[0] + suffix)
            if executable:
                break
    if executable and Path(executable).suffix.lower() in {".bat", ".cmd"}:
        command = subprocess.list2cmdline([executable, *argv[1:]])
        return ["cmd.exe", "/d", "/s", "/c", command]
    if executable:
        return [executable, *argv[1:]]
    return argv


def run(argv: list[str], *, cwd: Path = ROOT, capture: bool = False) -> str:
    effective = _command(argv)
    print(f"$ {' '.join(argv)}", flush=True)
    completed = subprocess.run(
        effective,
        cwd=cwd,
        text=True,
        check=False,
        stdout=subprocess.PIPE if capture else None,
        stderr=subprocess.STDOUT if capture else None,
    )
    output = completed.stdout or ""
    if capture and output:
        print(output, end="" if output.endswith("\n") else "\n", flush=True)
    if completed.returncode != 0:
        raise ContractError(f"command failed with exit {completed.returncode}: {' '.join(argv)}")
    return output


def require(condition: bool, message: str) -> None:
    if not condition:
        raise ContractError(message)


def last_line(output: str) -> str:
    lines = [line.strip() for line in output.splitlines() if line.strip()]
    return lines[-1] if lines else ""


def learn() -> None:
    app = ROOT / "learn/es/dart/app"
    run(["flutter", "pub", "get"], cwd=app)
    run(["dart", "format", "lib", "test"], cwd=app)
    diff = subprocess.run(["git", "diff", "--exit-code", "--", "lib", "test"], cwd=app, check=False)
    require(diff.returncode == 0, "Dart formatter changed tracked files")
    run(["flutter", "analyze"], cwd=app)
    run(["flutter", "test"], cwd=app)
    if os.name != "nt":
        run(["flutter", "build", "web", "--release"], cwd=app)


def patterns() -> None:
    sweep = ROOT / "src/Web/Dart/pattern_sweep.dart"
    mediator = ROOT / "src/Web/Dart/mediator.dart"
    sources = [str(sweep), str(mediator)]
    run(["dart", "format", "--output=none", "--set-exit-if-changed", *sources])
    run(["dart", "analyze", "--fatal-infos", "--fatal-warnings", *sources])
    require(
        last_line(run(["dart", "run", str(mediator)], capture=True)) == "Dart Mediator: passed",
        "Dart Mediator canonical output mismatch",
    )
    require(
        last_line(run(["dart", "run", str(sweep)], capture=True)) == "Dart pattern sweep: 39/39 examples passed",
        "Dart aggregate output mismatch",
    )


def main() -> int:
    parser = argparse.ArgumentParser(description="Dart contracts extracted from legacy workflows.")
    parser.add_argument("surface", choices=["learn", "patterns"])
    args = parser.parse_args()
    if args.surface == "learn":
        learn()
    else:
        patterns()
    print(f"Dart clean-slate contract: PASS surface={args.surface}", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ContractError as exc:
        print(f"Dart clean-slate contract failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
