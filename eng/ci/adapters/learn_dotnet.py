#!/usr/bin/env python3
from __future__ import annotations

import os
import platform
import subprocess
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]


class ContractError(RuntimeError):
    pass


def run(argv: list[str], *, cwd: Path = ROOT, capture: bool = False) -> subprocess.CompletedProcess[str]:
    print(f"$ {' '.join(argv)}", flush=True)
    completed = subprocess.run(argv, cwd=cwd, text=True, capture_output=capture, check=False)
    if capture:
        if completed.stdout:
            print(completed.stdout, end="", flush=True)
        if completed.stderr:
            print(completed.stderr, end="", file=sys.stderr, flush=True)
    if completed.returncode != 0:
        raise ContractError(f"command failed with exit {completed.returncode}: {' '.join(argv)}")
    return completed


def run_csharp() -> None:
    api = ROOT / "learn/es/csharp/app/src/StockFlow.Api/StockFlow.Api.csproj"
    tests = ROOT / "learn/es/csharp/app/tests/StockFlow.Api.Tests/StockFlow.Api.Tests.csproj"

    run(["dotnet", "restore", str(api)])
    run(["dotnet", "restore", str(tests)])
    run(["dotnet", "build", str(api), "--configuration", "Release", "--no-restore"])
    run(["dotnet", "test", str(tests), "--configuration", "Release", "--no-restore"])

    command = [
        "dotnet", "run", "--project", str(api), "--configuration", "Release", "--no-build",
        "--urls", "http://127.0.0.1:5073",
    ]
    print(f"$ {' '.join(command)}", flush=True)
    process = subprocess.Popen(command, cwd=ROOT, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    try:
        deadline = time.monotonic() + 20
        last_error = ""
        while time.monotonic() < deadline:
            if process.poll() is not None:
                output = process.stdout.read() if process.stdout else ""
                raise ContractError(f"StockFlow exited before health check passed.\n{output}")
            try:
                with urllib.request.urlopen("http://127.0.0.1:5073/health", timeout=1) as response:
                    body = response.read().decode("utf-8", errors="replace")
                    if response.status == 200:
                        print(body, flush=True)
                        return
            except (urllib.error.URLError, TimeoutError) as exc:
                last_error = str(exc)
            time.sleep(1)
        raise ContractError(f"StockFlow did not become healthy in time: {last_error}")
    finally:
        if process.poll() is None:
            process.terminate()
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=5)


def run_fsharp() -> None:
    app = ROOT / "learn/es/fsharp/app/QuoteRules/QuoteRules.fsproj"
    tests = ROOT / "learn/es/fsharp/app/QuoteRules.Tests/QuoteRules.Tests.fsproj"
    results = ROOT / "learn/es/fsharp/TestResults"

    run(["dotnet", "build", str(app), "--configuration", "Release"])
    run([
        "dotnet", "test", str(tests), "--configuration", "Release",
        '--collect:XPlat Code Coverage', "--results-directory", str(results),
    ])
    run([sys.executable, str(ROOT / "learn/es/fsharp/tools/check_coverage.py")])
    run(["dotnet", "run", "--project", str(app), "--configuration", "Release"])


def run_vb_core() -> None:
    app = ROOT / "learn/es/vbnet/app"
    run(["dotnet", "test", "QuoteDesk.Tests/QuoteDesk.Tests.vbproj", "-c", "Release"], cwd=app)


def run_vb_windows() -> None:
    app = ROOT / "learn/es/vbnet/app"
    project = "QuoteDesk.WinForms/QuoteDesk.WinForms.vbproj"
    run(["dotnet", "build", project, "-c", "Release"], cwd=app)
    publish = app / "publish/quotedesk"
    run([
        "dotnet", "publish", project, "-c", "Release", "-r", "win-x64",
        "--self-contained", "false", "-o", str(publish),
    ], cwd=app)
    executable = publish / "QuoteDesk.WinForms.exe"
    if not executable.exists():
        raise ContractError(f"No se generó {executable}")


def main() -> int:
    current = platform.system().lower()
    print(f"Learn .NET platform: {platform.platform()}", flush=True)
    run(["dotnet", "--info"])

    if current == "windows":
        run_fsharp()
        run_vb_core()
        run_vb_windows()
        print("Learn .NET Windows contracts: PASS", flush=True)
        return 0

    run_csharp()
    run_fsharp()
    run_vb_core()
    print("Learn .NET Linux contracts: PASS", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ContractError as exc:
        print(f"Learn .NET contract failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
