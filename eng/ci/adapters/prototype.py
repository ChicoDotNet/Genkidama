#!/usr/bin/env python3
from __future__ import annotations

import argparse
import shutil
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
EXPECTED_ORIGINAL = "original=orders: metrics"
EXPECTED_CLONE = "clone=orders-canary: metrics,tracing"
FORBIDDEN_SHARED_STATE = "original=orders: metrics,tracing"


def run(argv: list[str], *, cwd: Path | None = None) -> str:
    completed = subprocess.run(
        argv,
        cwd=cwd or ROOT,
        text=True,
        capture_output=True,
        check=False,
    )
    if completed.stdout:
        print(completed.stdout, end="" if completed.stdout.endswith("\n") else "\n")
    if completed.stderr:
        print(completed.stderr, end="" if completed.stderr.endswith("\n") else "\n")
    if completed.returncode != 0:
        raise SystemExit(completed.returncode)
    return completed.stdout + completed.stderr


def assert_output(output: str) -> None:
    if EXPECTED_ORIGINAL not in output:
        raise SystemExit(f"Prototype contract missing: {EXPECTED_ORIGINAL}")
    if EXPECTED_CLONE not in output:
        raise SystemExit(f"Prototype contract missing: {EXPECTED_CLONE}")
    if FORBIDDEN_SHARED_STATE in output:
        raise SystemExit("Prototype clone shares mutable feature state with the original")


def validate_dotnet() -> None:
    source = ROOT / "src/Enterprise/C#/PrototypeExample.cs"
    with tempfile.TemporaryDirectory(prefix="genkidama-prototype-csharp-") as temp:
        work = Path(temp)
        run(["dotnet", "new", "console", "--framework", "net10.0", "--output", str(work), "--force"])
        shutil.copyfile(source, work / "Program.cs")
        assert_output(run(["dotnet", "run", "--project", str(work), "--configuration", "Release"]))


def validate_jvm() -> None:
    source = ROOT / "src/Enterprise/Java/PrototypeExample.java"
    with tempfile.TemporaryDirectory(prefix="genkidama-prototype-java-") as temp:
        work = Path(temp)
        run(["javac", "-Xlint:all", "-Werror", "-d", str(work), str(source)])
        assert_output(run(["java", "-cp", str(work), "PrototypeExample"]))


def validate_native() -> None:
    source = ROOT / "src/Systems/Rust/prototype.rs"
    with tempfile.TemporaryDirectory(prefix="genkidama-prototype-rust-") as temp:
        binary = Path(temp) / ("prototype.exe" if __import__("os").name == "nt" else "prototype")
        run(["rustfmt", "--check", str(source)])
        run(["rustc", "-D", "warnings", str(source), "-o", str(binary)])
        assert_output(run([str(binary)]))


def main() -> int:
    parser = argparse.ArgumentParser(description="Validate the Prototype behavior contract in representative runtimes.")
    parser.add_argument("family", choices=["dotnet", "jvm", "native"])
    args = parser.parse_args()
    validators = {
        "dotnet": validate_dotnet,
        "jvm": validate_jvm,
        "native": validate_native,
    }
    validators[args.family]()
    print(f"Prototype {args.family} contract: OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
