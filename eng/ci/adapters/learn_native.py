#!/usr/bin/env python3
from __future__ import annotations

import os
import shutil
import subprocess
import sys
import tempfile
import time
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
PROFILE = os.environ.get("GENKIDAMA_NATIVE_PROFILE", "").strip().lower()


class ContractError(RuntimeError):
    pass


def run(
    argv: list[str],
    *,
    cwd: Path = ROOT,
    env: dict[str, str] | None = None,
    capture: bool = False,
) -> str:
    print(f"$ {' '.join(argv)}", flush=True)
    completed = subprocess.run(
        argv,
        cwd=cwd,
        env=env,
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


def require_contains(label: str, text: str, expected: str) -> None:
    if expected not in text:
        raise ContractError(f"{label} output missing {expected!r}: {text!r}")


def c_contract(cc: str) -> None:
    app = ROOT / "learn/es/c/app"
    run([cc, "--version"])
    run(["cmake", "--version"])
    run(["ninja", "--version"])

    with tempfile.TemporaryDirectory(prefix=f"genkidama-c-{cc}-") as temp:
        work = Path(temp)
        build = work / "build"
        dist = work / "dist"
        run([
            "cmake",
            "-S",
            str(app),
            "-B",
            str(build),
            "-G",
            "Ninja",
            "-DCMAKE_BUILD_TYPE=Release",
            f"-DCMAKE_C_COMPILER={cc}",
        ])
        run(["cmake", "--build", str(build), "--parallel"])
        run(["ctest", "--test-dir", str(build), "--output-on-failure"])
        run(["cmake", "--install", str(build), "--prefix", str(dist)])

        sample = work / "sample.gtl"
        recovered = work / "recovered.gtl"
        cli = build / "telemetry_cli"
        run([str(cli), "init", str(sample)])
        run([str(cli), "log", str(sample), "1000", "7", "21500", "0"])
        require_contains(
            "C telemetry list",
            run([str(cli), "list", str(sample)], capture=True),
            "sensor=7 value_milli=21500 status=0",
        )
        require_contains(
            "C telemetry diagnose",
            run([str(cli), "diagnose", str(sample)], capture=True),
            "Estado: ok",
        )
        with sample.open("ab") as stream:
            stream.write(b"bad")
        run([str(cli), "recover", str(sample), str(recovered)])
        require_contains(
            "C telemetry recovered diagnose",
            run([str(cli), "diagnose", str(recovered)], capture=True),
            "Estado: ok",
        )
        if not (dist / "bin/telemetry_cli").is_file():
            raise ContractError("installed C telemetry_cli is missing")
        if not (dist / "include/telemetry.h").is_file():
            raise ContractError("installed C telemetry.h is missing")
    print(f"C Learn contract: PASS compiler={cc}", flush=True)


def cpp_linux_contract(cxx: str) -> None:
    app = ROOT / "learn/es/cpp/app"
    run([cxx, "--version"])
    run(["cmake", "--version"])
    run(["ninja", "--version"])

    with tempfile.TemporaryDirectory(prefix=f"genkidama-cpp-{cxx.replace('+', 'p')}-") as temp:
        work = Path(temp)
        build = work / "build"
        run([
            "cmake",
            "-S",
            str(app),
            "-B",
            str(build),
            "-G",
            "Ninja",
            "-DCMAKE_BUILD_TYPE=Release",
            f"-DCMAKE_CXX_COMPILER={cxx}",
        ])
        run(["cmake", "--build", str(build), "--parallel"])
        run(["ctest", "--test-dir", str(build), "--output-on-failure"])

        smoke = work / "threadseek-smoke"
        (smoke / "docs").mkdir(parents=True)
        (smoke / "README.md").write_text("hola\n", encoding="utf-8")
        (smoke / "docs/manual.txt").write_text("manual\n", encoding="utf-8")
        run([str(build / "threadseek"), str(smoke), "txt"])
    print(f"C++ Learn contract: PASS compiler={cxx}", flush=True)


def cpp_msvc_contract() -> None:
    if os.name != "nt":
        raise ContractError("MSVC profile requires Windows")
    app = ROOT / "learn/es/cpp/app"
    run(["cmake", "--version"])

    with tempfile.TemporaryDirectory(prefix="genkidama-cpp-msvc-") as temp:
        work = Path(temp)
        build = work / "build"
        run(["cmake", "-S", str(app), "-B", str(build), "-A", "x64"])
        run(["cmake", "--build", str(build), "--config", "Release", "--parallel"])
        run(["ctest", "--test-dir", str(build), "-C", "Release", "--output-on-failure"])

        smoke = work / "threadseek-smoke"
        (smoke / "docs").mkdir(parents=True)
        (smoke / "README.md").write_text("hola\n", encoding="utf-8")
        (smoke / "docs/manual.txt").write_text("manual\n", encoding="utf-8")
        exe = build / "Release/threadseek.exe"
        if not exe.is_file():
            raise ContractError(f"MSVC threadseek executable is missing: {exe}")
        run([str(exe), str(smoke), "txt"])
    print("C++ Learn contract: PASS compiler=msvc", flush=True)


def rust_contract() -> None:
    app = ROOT / "learn/es/rust/app"
    run(["rustc", "--version"])
    run(["cargo", "--version"])
    run(["cargo", "fmt", "--check"], cwd=app)
    run(["cargo", "clippy", "--all-targets", "--all-features", "--", "-D", "warnings"], cwd=app)
    run(["cargo", "test"], cwd=app)
    run(["cargo", "build", "--release"], cwd=app)

    with tempfile.TemporaryDirectory(prefix="genkidama-rust-smoke-") as temp:
        work = Path(temp)
        source = work / "source"
        (source / "docs").mkdir(parents=True)
        (source / "hello.txt").write_text("hola rust\n", encoding="utf-8")
        (source / "docs/readme.txt").write_text("backup verificable\n", encoding="utf-8")
        backup = work / "backup"
        run(["cargo", "run", "--quiet", "--", "create", str(source), str(backup)], cwd=app)
        run(["cargo", "run", "--quiet", "--", "verify", str(backup)], cwd=app)
    print("Rust Learn contract: PASS", flush=True)


def go_contract() -> None:
    app = ROOT / "learn/es/go/app"
    run(["go", "version"])
    unformatted = run(["gofmt", "-l", "."], cwd=app, capture=True).strip()
    if unformatted:
        raise ContractError(f"Go Learn files are not gofmt-clean:\n{unformatted}")
    run(["go", "vet", "./..."], cwd=app)
    run(["go", "test", "-race", "./..."], cwd=app)

    with tempfile.TemporaryDirectory(prefix="genkidama-go-smoke-") as temp:
        work = Path(temp)
        binary = work / ("uptimelab.exe" if os.name == "nt" else "uptimelab")
        run(["go", "build", "-o", str(binary), "./cmd/uptimelab"], cwd=app)

        env = os.environ.copy()
        env["UPTIMELAB_TARGETS"] = "Local=http://127.0.0.1:65534"
        env["UPTIMELAB_ADDR"] = "127.0.0.1:18080"
        log_path = work / "uptimelab.log"
        print(f"$ {binary}", flush=True)
        with log_path.open("w", encoding="utf-8") as log:
            process = subprocess.Popen(
                [str(binary)],
                cwd=app,
                env=env,
                stdout=log,
                stderr=subprocess.STDOUT,
                text=True,
            )
            try:
                for _ in range(20):
                    if process.poll() is not None:
                        break
                    try:
                        with urllib.request.urlopen("http://127.0.0.1:18080/health", timeout=1) as response:
                            if response.status == 200:
                                print("Go UptimeLab smoke: PASS", flush=True)
                                return
                    except Exception:
                        time.sleep(0.25)
                if log_path.exists():
                    print(log_path.read_text(encoding="utf-8"), file=sys.stderr)
                raise ContractError("Go UptimeLab process did not become healthy in time")
            finally:
                if process.poll() is None:
                    process.terminate()
                    try:
                        process.wait(timeout=5)
                    except subprocess.TimeoutExpired:
                        process.kill()
                        process.wait(timeout=5)


def main() -> int:
    if PROFILE == "gnu":
        c_contract("gcc")
        cpp_linux_contract("g++")
    elif PROFILE == "clang":
        c_contract("clang")
        cpp_linux_contract("clang++")
    elif PROFILE == "rust":
        rust_contract()
    elif PROFILE == "go":
        go_contract()
    elif PROFILE == "msvc":
        cpp_msvc_contract()
    else:
        raise ContractError("GENKIDAMA_NATIVE_PROFILE must be one of: gnu, clang, rust, go, msvc")
    print(f"Native Learn contract: PASS profile={PROFILE}", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ContractError as exc:
        print(f"Native Learn contract failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
