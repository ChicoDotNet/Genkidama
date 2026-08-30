#!/usr/bin/env python3
from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
EXPECTED_ORIGINAL = "original=orders: metrics"
EXPECTED_CLONE = "clone=orders-canary: metrics,tracing"
FORBIDDEN_SHARED_STATE = "original=orders: metrics,tracing"


def run(argv: list[str], *, cwd: Path | None = None) -> str:
    completed = subprocess.run(argv, cwd=cwd or ROOT, text=True, capture_output=True, check=False)
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


def validate_csharp() -> None:
    source = ROOT / "src/Enterprise/C#/PrototypeExample.cs"
    with tempfile.TemporaryDirectory(prefix="genkidama-prototype-csharp-") as temp:
        work = Path(temp)
        run(["dotnet", "new", "console", "--framework", "net10.0", "--output", str(work), "--force"])
        shutil.copyfile(source, work / "Program.cs")
        assert_output(run(["dotnet", "run", "--project", str(work), "--configuration", "Release"]))


def validate_fsharp() -> None:
    assert_output(run(["dotnet", "fsi", "src/Functional/F#/prototype.fsx"]))


def validate_java() -> None:
    source = ROOT / "src/Enterprise/Java/PrototypeExample.java"
    with tempfile.TemporaryDirectory(prefix="genkidama-prototype-java-") as temp:
        work = Path(temp)
        run(["javac", "-Xlint:all", "-Werror", "-d", str(work), str(source)])
        assert_output(run(["java", "-cp", str(work), "PrototypeExample"]))


def validate_kotlin() -> None:
    source = ROOT / "src/Enterprise/Kotlin/PrototypeExample.kt"
    with tempfile.TemporaryDirectory(prefix="genkidama-prototype-kotlin-") as temp:
        jar = Path(temp) / "prototype.jar"
        run(["kotlinc", str(source), "-include-runtime", "-d", str(jar)])
        assert_output(run(["java", "-jar", str(jar)]))


def validate_rust() -> None:
    source = ROOT / "src/Systems/Rust/prototype.rs"
    with tempfile.TemporaryDirectory(prefix="genkidama-prototype-rust-") as temp:
        binary = Path(temp) / ("prototype.exe" if os.name == "nt" else "prototype")
        run(["rustfmt", "--check", str(source)])
        run(["rustc", "-D", "warnings", str(source), "-o", str(binary)])
        assert_output(run([str(binary)]))


def validate_cpp() -> None:
    source = ROOT / "src/Systems/C++/prototype.cpp"
    with tempfile.TemporaryDirectory(prefix="genkidama-prototype-cpp-") as temp:
        binary = Path(temp) / "prototype-cpp"
        compiler = shutil.which("g++-14") or shutil.which("g++") or "g++"
        run([compiler, "-std=c++20", "-Wall", "-Wextra", "-Werror", str(source), "-o", str(binary)])
        assert_output(run([str(binary)]))


def validate_go() -> None:
    source = ROOT / "src/Systems/Go/prototype.go"
    if run(["gofmt", "-d", str(source)]).strip():
        raise SystemExit("Go Prototype is not gofmt-clean")
    run(["go", "vet", str(source)])
    assert_output(run(["go", "run", str(source)]))


def validate_javascript() -> None:
    source = ROOT / "src/Web/JavaScriptJS/prototype.js"
    run(["node", "--check", str(source)])
    assert_output(run(["node", str(source)]))


def validate_typescript() -> None:
    source = ROOT / "src/Web/TypeScriptTS/prototype.ts"
    with tempfile.TemporaryDirectory(prefix="genkidama-prototype-ts-") as temp:
        out = Path(temp)
        run(["npx", "--yes", "--package", "typescript@6.0.3", "tsc", str(source), "--strict", "--target", "ES2024", "--module", "commonjs", "--outDir", str(out)])
        assert_output(run(["node", str(out / "prototype.js")]))


def validate_python() -> None:
    source = ROOT / "src/Scripting/PythonPY/prototype.py"
    run(["python", "-m", "py_compile", str(source)])
    assert_output(run(["python", str(source)]))


def validate_php() -> None:
    source = ROOT / "src/Scripting/PHP/prototype.php"
    run(["php", "-l", str(source)])
    assert_output(run(["php", str(source)]))


def validate_swift() -> None:
    source = ROOT / "src/Systems/Swift/prototype.swift"
    with tempfile.TemporaryDirectory(prefix="genkidama-prototype-swift-") as temp:
        binary = Path(temp) / "prototype-swift"
        run(["swiftc", str(source), "-o", str(binary)])
        assert_output(run([str(binary)]))


def main() -> int:
    parser = argparse.ArgumentParser(description="Validate Prototype behavior contracts across runtime families.")
    parser.add_argument("family", choices=["dotnet", "jvm", "native", "web", "scripting", "swift"])
    args = parser.parse_args()
    if args.family == "dotnet":
        validate_csharp(); validate_fsharp()
    elif args.family == "jvm":
        profile = os.environ.get("GENKIDAMA_JVM_PROFILE", "java25").lower()
        if profile == "java25": validate_java()
        elif profile == "jvm17": validate_kotlin()
        else: raise SystemExit(f"Unsupported JVM profile for Prototype: {profile}")
    elif args.family == "native":
        profile = os.environ.get("GENKIDAMA_NATIVE_PROFILE", "").lower()
        if profile == "gnu": validate_cpp()
        elif profile == "rust": validate_rust()
        elif profile == "go": validate_go()
        elif profile in {"clang", "msvc"}: pass
        else: raise SystemExit(f"Unsupported native profile for Prototype: {profile}")
    elif args.family == "web":
        validate_javascript(); validate_typescript()
    elif args.family == "scripting":
        if os.environ.get("GENKIDAMA_SCRIPTING_PROFILE", "linux").lower() != "windows":
            validate_python(); validate_php()
    elif args.family == "swift":
        validate_swift()
    print(f"Prototype {args.family} contract: OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
