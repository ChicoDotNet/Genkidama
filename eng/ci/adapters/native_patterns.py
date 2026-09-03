#!/usr/bin/env python3
from __future__ import annotations

import os
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
PROFILE = os.environ.get("GENKIDAMA_NATIVE_PROFILE", "").strip().lower()
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
    return completed.stdout or ""


def exact_files(path: Path, suffix: str, label: str) -> list[Path]:
    files = sorted(path.glob(f"*{suffix}"))
    if len(files) != EXPECTED:
        raise ContractError(f"{label} pattern cell count is {len(files)}; expected {EXPECTED}")
    return files


def validate_c_cpp() -> int:
    c_files = exact_files(ROOT / "src/Systems/C/patterns", ".c", "C")
    cpp_files = exact_files(ROOT / "src/Systems/C++/patterns", ".cpp", "C++")
    c_compiler = "gcc-14"
    cpp_compiler = "g++-14"
    run([c_compiler, "--version"])
    run([cpp_compiler, "--version"])

    with tempfile.TemporaryDirectory(prefix="genkidama-native-patterns-") as temp:
        work = Path(temp)
        for source in c_files:
            cell_source = work / "cell.c"
            cell_binary = work / "cell-c"
            cell_source.write_text(
                source.read_text(encoding="utf-8") + "\nint main(void){return run()?0:1;}\n",
                encoding="utf-8",
            )
            run([c_compiler, "-std=c23", "-Wall", "-Wextra", "-Werror", str(cell_source), "-o", str(cell_binary)])
            run([str(cell_binary)])
            print(f"PASS C {source.name}", flush=True)

        for source in cpp_files:
            cell_source = work / "cell.cpp"
            cell_binary = work / "cell-cpp"
            cell_source.write_text(
                source.read_text(encoding="utf-8") + "\nint main(){return run()?0:1;}\n",
                encoding="utf-8",
            )
            run([cpp_compiler, "-std=c++23", "-Wall", "-Wextra", "-Werror", str(cell_source), "-o", str(cell_binary)])
            run([str(cell_binary)])
            print(f"PASS C++ {source.name}", flush=True)

    print(f"C pattern cells: {EXPECTED}/{EXPECTED} passed", flush=True)
    print(f"C++ pattern cells: {EXPECTED}/{EXPECTED} passed", flush=True)
    return EXPECTED * 2


def validate_rust() -> int:
    files = exact_files(ROOT / "src/Systems/Rust/patterns", ".rs", "Rust")
    run(["rustc", "--version"])

    with tempfile.TemporaryDirectory(prefix="genkidama-rust-patterns-") as temp:
        work = Path(temp)
        for source in files:
            cell_source = work / "cell.rs"
            cell_binary = work / ("cell.exe" if os.name == "nt" else "cell")
            cell_source.write_text(
                source.read_text(encoding="utf-8") + "\nfn main(){assert!(run());}\n",
                encoding="utf-8",
            )
            run(["rustc", "--edition=2024", "-D", "warnings", str(cell_source), "-o", str(cell_binary)])
            run([str(cell_binary)])
            print(f"PASS Rust {source.name}", flush=True)

    print(f"Rust pattern cells: {EXPECTED}/{EXPECTED} passed", flush=True)
    return EXPECTED


def validate_go() -> int:
    source = ROOT / "src/Systems/Go/pattern_sweep.go"
    observer = ROOT / "src/Systems/Go/observer.go"
    if not source.is_file():
        raise ContractError("Go pattern_sweep.go is missing")
    if not observer.is_file():
        raise ContractError("Go Observer canonical is missing")
    run(["go", "version"])
    unformatted = run(["gofmt", "-l", str(source), str(observer)], capture=True).strip()
    if unformatted:
        raise ContractError(f"Go sources are not gofmt-clean: {unformatted}")
    run(["go", "vet", str(source), str(observer)])

    output = run(["go", "run", str(source), str(observer)], capture=True).strip()
    expected = "Go pattern sweep: 39/39 examples passed"
    if output != expected:
        raise ContractError(f"Go pattern sweep output mismatch: expected {expected!r}, got {output!r}")
    print(output, flush=True)

    verifier = observer.parent / "observer_verify_tmp.go"
    verifier.write_text(
        'package main\n\nimport "fmt"\n\nfunc main() {\n\tif !observerExamplePasses() { panic("Observer canonical failed") }\n\tfmt.Println("Go Observer: passed")\n}\n',
        encoding="utf-8",
    )
    try:
        unformatted_verifier = run(["gofmt", "-l", str(verifier)], capture=True).strip()
        if unformatted_verifier:
            run(["gofmt", "-w", str(verifier)])
        run(["go", "vet", str(observer), str(verifier)])
        observer_output = run(["go", "run", str(observer), str(verifier)], capture=True).strip()
        if observer_output != "Go Observer: passed":
            raise ContractError(f"Go Observer canonical output mismatch: {observer_output!r}")
        print(observer_output, flush=True)
    finally:
        verifier.unlink(missing_ok=True)

    return EXPECTED + 1


def main() -> int:
    if PROFILE == "gnu":
        total = validate_c_cpp()
    elif PROFILE == "rust":
        total = validate_rust()
    elif PROFILE == "go":
        total = validate_go()
    else:
        raise ContractError("Native Patterns requires GENKIDAMA_NATIVE_PROFILE=gnu, rust, or go")
    print(f"Native Patterns contract: PASS profile={PROFILE} validations={total}", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ContractError as exc:
        print(f"Native Patterns contract failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
