#!/usr/bin/env python3
from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import tempfile
import time
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
EXPECTED = 39


class ContractError(RuntimeError):
    pass


def run(argv: list[str], *, cwd: Path = ROOT, env: dict[str, str] | None = None, capture: bool = False) -> str:
    merged = os.environ.copy()
    if env:
        merged.update(env)
    print(f"$ {' '.join(argv)}", flush=True)
    completed = subprocess.run(argv, cwd=cwd, env=merged, text=True, check=False, stdout=subprocess.PIPE if capture else None, stderr=subprocess.STDOUT if capture else None)
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


def exact_glob(directory: Path, pattern: str, label: str) -> list[Path]:
    files = sorted(directory.glob(pattern))
    require(len(files) == EXPECTED, f"{label} cell count is {len(files)}; expected {EXPECTED}")
    return files


def wait_http(url: str, attempts: int = 20) -> tuple[bytes, dict[str, str]]:
    error: Exception | None = None
    for _ in range(attempts):
        try:
            with urllib.request.urlopen(url, timeout=2) as response:
                return response.read(), {key.lower(): value for key, value in response.headers.items()}
        except Exception as exc:
            error = exc
            time.sleep(0.25)
    raise ContractError(f"HTTP smoke failed for {url}: {error}")


def learn_git() -> None:
    run(["bash", "learn/es/git/tools/verify-core.sh"])
    placeholders = []
    for path in (ROOT / "learn/es/git").rglob("*.md"):
        if "PLACEHOLDER" in path.read_text(encoding="utf-8"):
            placeholders.append(str(path.relative_to(ROOT)))
    require(not placeholders, f"unfinished Git course placeholders: {placeholders}")
    run(["bash", "learn/es/git/tools/verify-advanced.sh"])


def learn_web() -> None:
    js = ROOT / "learn/es/javascript/app"
    run(["npm", "run", "verify"], cwd=js)
    proc = subprocess.Popen(["npm", "start"], cwd=js, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    try:
        body, headers = wait_http("http://127.0.0.1:4173/")
        html = body.decode("utf-8")
        require("<h1>Kanban Local</h1>" in html, "JavaScript smoke missing Kanban heading")
        require("content-security-policy" in headers, "JavaScript smoke missing CSP header")
        require(headers.get("x-content-type-options", "").lower() == "nosniff", "JavaScript smoke missing nosniff")
        app, _ = wait_http("http://127.0.0.1:4173/src/app.js")
        require(b"applyBoardCommand" in app, "JavaScript smoke missing applyBoardCommand")
        manifest, manifest_headers = wait_http("http://127.0.0.1:4173/manifest.webmanifest")
        require(b'"display": "standalone"' in manifest, "JavaScript manifest is not standalone")
        require("application/manifest+json" in manifest_headers.get("content-type", ""), "JavaScript manifest content type mismatch")
        worker, _ = wait_http("http://127.0.0.1:4173/service-worker.js")
        require(b"kanban-local-v2" in worker, "JavaScript service worker cache contract missing")
    finally:
        proc.terminate()
        try:
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            proc.kill()

    ts = ROOT / "learn/es/typescript/app"
    run(["npm", "install", "--no-audit", "--no-fund"], cwd=ts)
    run(["npm", "run", "verify"], cwd=ts)
    proc = subprocess.Popen(["node", "dist/src/server/index.js"], cwd=ts, env={**os.environ, "PORT": "3100"}, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    try:
        wait_http("http://127.0.0.1:3100/")
    finally:
        proc.terminate()
        try:
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            proc.kill()

    solidity = ROOT / "learn/es/solidity"
    for rel in ["lessons/17-evaluacion-final.md", "exercises/evaluacion-final.md", "exercises/rubrica-final.md", "solutions/evaluacion-final.md"]:
        require((solidity / rel).is_file(), f"Solidity final course package missing {rel}")
    run(["forge", "fmt", "--check", "app/src/FreelanceEscrow.sol", "app/test/FreelanceEscrow.t.sol", "app/test/Security.t.sol"], cwd=solidity)
    run(["forge", "build"], cwd=solidity)
    run(["forge", "test", "-vv"], cwd=solidity)


def patterns_web() -> None:
    js_files = exact_glob(ROOT / "src/Web/JavaScriptJS/patterns", "*.js", "JavaScript")
    for source in js_files:
        run(["node", "--check", str(source)])
        run(["node", str(source)])
    aggregate = ROOT / "src/Web/JavaScriptJS/pattern_sweep.js"
    run(["node", "--check", str(aggregate)])
    require(last_line(run(["node", str(aggregate)], capture=True)) == "javascript-pattern-sweep: 39/39 passed", "JavaScript aggregate output mismatch")

    with tempfile.TemporaryDirectory(prefix="genkidama-web-patterns-") as temp:
        work = Path(temp)
        ts_source = work / "typescript-sweep.ts"
        parts = [p.read_text(encoding="utf-8") for p in sorted((ROOT / "src/Web/TypeScriptTS/patterns").glob("*.ts"))]
        require(len(parts) == EXPECTED, f"TypeScript cell count is {len(parts)}; expected {EXPECTED}")
        parts.append((ROOT / "src/Web/TypeScriptTS/pattern-sweep.ts").read_text(encoding="utf-8"))
        ts_source.write_text("\n".join(parts), encoding="utf-8")
        run(["npx", "--yes", "--package", "typescript@latest", "tsc", "--noEmit", "--strict", "--target", "ES2022", str(ts_source)])
        require(last_line(run(["npx", "--yes", "tsx@latest", str(ts_source)], capture=True)) == "TypeScript pattern sweep: 39/39 examples passed", "TypeScript aggregate output mismatch")
        solidity_out = work / "solidity"
        solidity_out.mkdir()
        solidity = ROOT / "src/Niche/Solidity/PatternSweep.sol"
        run(["npx", "--yes", "solc@latest", "--bin", "--abi", str(solidity), "-o", str(solidity_out)])
        bins = list(solidity_out.glob("*PatternSweep.bin"))
        require(any(path.stat().st_size > 0 for path in bins), "Solidity PatternSweep bytecode missing")
        require("function runAll() external pure returns(uint passed)" in solidity.read_text(encoding="utf-8"), "Solidity runAll source contract missing")
    run([sys.executable, "eng/ci/adapters/prototype.py", "web"])


def powershell_contract() -> None:
    tests = ROOT / "learn/es/powershell/app/tests"
    run(["pwsh", "-NoLogo", "-NoProfile", "-Command", f"Invoke-Pester -Path '{tests}' -CI -Output Detailed"])
    with tempfile.TemporaryDirectory(prefix="genkidama-ps-learn-") as temp:
        report = Path(temp) / "workstation-audit.json"
        command = f"& '{ROOT / 'learn/es/powershell/app/Invoke-Audit.ps1'}' -OutputPath '{report}'; if (-not (Test-Path -LiteralPath '{report}')) {{ throw 'No se generó el reporte.' }}; Get-Content -LiteralPath '{report}' -Raw | ConvertFrom-Json | Out-Null"
        run(["pwsh", "-NoLogo", "-NoProfile", "-Command", command])


def ruby_learn() -> None:
    ruby = ROOT / "learn/es/ruby/app"
    run(["bundle", "install"], cwd=ruby)
    env = {"RAILS_ENV": "test"}
    run(["bundle", "exec", "rails", "db:prepare"], cwd=ruby, env=env)
    run(["bundle", "exec", "rails", "test"], cwd=ruby, env=env)
    run(["bundle", "exec", "rails", "runner", "script/smoke.rb"], cwd=ruby, env=env)


def learn_scripting() -> None:
    profile = os.environ.get("GENKIDAMA_SCRIPTING_PROFILE", "linux").lower()
    require(profile in {"linux", "windows"}, f"unsupported scripting profile: {profile}")
    ruby_learn()
    powershell_contract()
    if profile == "windows":
        return
    py = ROOT / "learn/es/python/app"
    run([sys.executable, "-m", "pip", "install", "--disable-pip-version-check", "-e", ".[dev]"], cwd=py)
    run([sys.executable, "-m", "compileall", "-q", "src"], cwd=py)
    run([sys.executable, "-m", "pytest"], cwd=py)
    with tempfile.TemporaryDirectory(prefix="genkidama-python-wheel-") as temp:
        run([sys.executable, "-m", "pip", "wheel", "--disable-pip-version-check", "--no-deps", ".", "-w", temp], cwd=py)
    with tempfile.TemporaryDirectory(prefix="genkidama-ledgermatch-") as temp:
        db = str(Path(temp) / "ledgermatch.db")
        first = run(["ledgermatch", "examples/invoices.csv", "--db", db], cwd=py, capture=True)
        for marker in ["Procesadas: 4", "Con diferencia: 2", "Persistencia: importación #1 creada"]:
            require(marker in first, f"Python Learn first-run contract missing {marker!r}")
        second = run(["ledgermatch", "examples/invoices.csv", "--db", db], cwd=py, capture=True)
        require("Persistencia: importación #1 ya registrada" in second, "Python Learn idempotency contract missing")
    php = ROOT / "learn/es/php/app"
    run(["composer", "install", "--no-interaction", "--prefer-dist"], cwd=php)
    run(["bash", "tools/verify.sh"], cwd=php)
    run(["bash", "tools/smoke.sh"], cwd=php)


def patterns_scripting() -> None:
    profile = os.environ.get("GENKIDAMA_SCRIPTING_PROFILE", "linux").lower()
    if profile == "windows":
        return
    py = ROOT / "src/Scripting/PythonPY/pattern_sweep.py"
    run([sys.executable, "-m", "py_compile", str(py)])
    run([sys.executable, "-B", str(py)])
    ruby_files = exact_glob(ROOT / "src/Scripting/Ruby/patterns", "*.rb", "Ruby")
    for source in ruby_files:
        run(["ruby", "-c", str(source)])
        run(["ruby", str(source)])
    ruby_aggregate = ROOT / "src/Scripting/Ruby/pattern_sweep.rb"
    run(["ruby", "-c", str(ruby_aggregate)])
    require(last_line(run(["ruby", str(ruby_aggregate)], capture=True)) == "ruby-pattern-sweep: 39/39 passed", "Ruby aggregate output mismatch")
    php_files = exact_glob(ROOT / "src/Scripting/PHP/patterns", "*.php", "PHP")
    for source in php_files:
        run(["php", "-l", str(source)])
        run(["php", str(source)])
    php_aggregate = ROOT / "src/Scripting/PHP/pattern_sweep.php"
    run(["php", "-l", str(php_aggregate)])
    require(last_line(run(["php", str(php_aggregate)], capture=True)) == "php-pattern-sweep: 39/39 passed", "PHP aggregate output mismatch")
    ps_files = exact_glob(ROOT / "src/Scripting/PowerShell/patterns", "*.ps1", "PowerShell")
    for source in ps_files:
        parse = f"$tokens=$null; $errors=$null; [void][System.Management.Automation.Language.Parser]::ParseFile('{source}',[ref]$tokens,[ref]$errors); if ($errors.Count -gt 0) {{ $errors | ForEach-Object {{ Write-Error $_.Message }}; exit 1 }}"
        run(["pwsh", "-NoLogo", "-NoProfile", "-Command", parse])
        run(["pwsh", "-NoLogo", "-NoProfile", "-File", str(source)])
    run(["pwsh", "-NoLogo", "-NoProfile", "-File", str(ROOT / "src/Scripting/PowerShell/pattern_sweep.ps1")])
    bash_bin = os.environ.get("GENKIDAMA_BASH_BIN", "bash")
    bash_files = exact_glob(ROOT / "src/Scripting/Bash/patterns", "*.sh", "Bash")
    run([bash_bin, "-n", *map(str, bash_files), str(ROOT / "src/Scripting/Bash/pattern_sweep.sh")])
    for source in bash_files:
        run([bash_bin, str(source)])
    output = run([bash_bin, str(ROOT / "src/Scripting/Bash/pattern_sweep.sh")], capture=True)
    require("bash-pattern-sweep: 39/39 passed" in output.splitlines(), "Bash aggregate output mismatch")
    lua = os.environ.get("GENKIDAMA_LUA_BIN", "lua")
    luac = os.environ.get("GENKIDAMA_LUAC_BIN", "luac")
    lua_files = exact_glob(ROOT / "src/Scripting/Lua/patterns", "*.lua", "Lua")
    for source in lua_files:
        run([luac, "-p", str(source)])
        run([lua, str(source)])
    run([lua, str(ROOT / "src/Scripting/Lua/pattern_sweep.lua")])
    run([sys.executable, "eng/ci/adapters/prototype.py", "scripting"])


def learn_gnu() -> None:
    run(["bash", "tests/smoke.sh"], cwd=ROOT / "learn/es/cobol/app")


def patterns_gnu() -> None:
    with tempfile.TemporaryDirectory(prefix="genkidama-gnu-patterns-") as temp:
        work = Path(temp)
        (work / "ada").mkdir()
        (work / "pascal").mkdir()
        checks = [
            ("Ada", ["gnatmake", "-q", "-gnat2022", "-gnatwa", "-gnatwe", "-D", str(work / "ada"), "-o", str(work / "ada-sweep"), str(ROOT / "src/Systems/Ada/pattern_sweep.adb")], [str(work / "ada-sweep")], "Ada pattern sweep: 39/39 examples passed"),
            ("Pascal", ["fpc", "-O2", "-S2", "-vw", f"-FE{work / 'pascal'}", f"-FU{work / 'pascal'}", str(ROOT / "src/Systems/Pascal/pattern_sweep.pas")], [str(work / "pascal/pattern_sweep")], "Pascal pattern sweep: 39/39 examples passed"),
            ("COBOL", ["cobc", "-free", "-x", "-Wall", "-Werror", str(ROOT / "src/Historical/Cobol/pattern_sweep.cbl"), "-o", str(work / "cobol-sweep")], [str(work / "cobol-sweep")], "COBOL pattern sweep: 39/39 examples passed"),
            ("Fortran", ["gfortran", "-std=f2018", "-Wall", "-Wextra", "-Werror", str(ROOT / "src/Systems/Fortran/pattern_sweep.f90"), "-o", str(work / "fortran-sweep")], [str(work / "fortran-sweep")], "Fortran pattern sweep: 39/39 examples passed")]
        for label, compile_cmd, run_cmd, expected in checks:
            run(compile_cmd)
            require(last_line(run(run_cmd, capture=True)) == expected, f"{label} aggregate output mismatch")


def learn_dart() -> None:
    app = ROOT / "learn/es/dart/app"
    run(["flutter", "pub", "get"], cwd=app)
    run(["dart", "format", "lib", "test"], cwd=app)
    diff = subprocess.run(["git", "diff", "--exit-code", "--", "lib", "test"], cwd=app, check=False)
    require(diff.returncode == 0, "Dart formatter changed tracked files")
    run(["flutter", "analyze"], cwd=app)
    run(["flutter", "test"], cwd=app)
    if os.name != "nt":
        run(["flutter", "build", "web", "--release"], cwd=app)


def patterns_dart() -> None:
    source = ROOT / "src/Web/Dart/pattern_sweep.dart"
    run(["dart", "format", "--output=none", "--set-exit-if-changed", str(source)])
    run(["dart", "analyze", "--fatal-infos", "--fatal-warnings", str(source)])
    require(last_line(run(["dart", "run", str(source)], capture=True)) == "Dart pattern sweep: 39/39 examples passed", "Dart aggregate output mismatch")


def learn_swift() -> None:
    app = ROOT / "learn/es/swift/app"
    run(["swift", "test"], cwd=app)
    run(["swift", "run", "TimeQuote"], cwd=app)


def patterns_swift() -> None:
    run(["swift", str(ROOT / "src/Systems/Swift/pattern_sweep.swift")])
    run([sys.executable, "eng/ci/adapters/prototype.py", "swift"])


def patterns_longtail() -> None:
    with tempfile.TemporaryDirectory(prefix="genkidama-longtail-") as temp:
        work = Path(temp)
        hs = ROOT / "src/Functional/Haskell/PatternSweep.hs"
        run(["ghc", "-Wall", "-Werror", "-O0", "-odir", str(work), "-hidir", str(work), str(hs), "-o", str(work / "haskell")])
        require(last_line(run([str(work / "haskell")], capture=True)) == "Haskell pattern sweep: 39/39 examples passed", "Haskell aggregate output mismatch")
        crystal = ROOT / "src/Niche/Crystal/pattern_sweep.cr"
        run(["crystal", "tool", "format", "--check", str(crystal)])
        run(["crystal", "build", "--error-on-warnings", str(crystal), "-o", str(work / "crystal")])
        require(last_line(run([str(work / "crystal")], capture=True)) == "Crystal pattern sweep: 39/39 examples passed", "Crystal aggregate output mismatch")
        zig = ROOT / "src/Systems/Zig/pattern_sweep.zig"
        run(["zig", "fmt", "--check", str(zig)])
        require(last_line(run(["zig", "run", str(zig)], capture=True)) == "Zig pattern sweep: 39/39 examples passed", "Zig aggregate output mismatch")
        julia = ROOT / "src/DataScience/Julia/pattern_sweep.jl"
        require(last_line(run(["julia", "--startup-file=no", "--check-bounds=yes", str(julia)], capture=True)) == "Julia pattern sweep: 39/39 examples passed", "Julia aggregate output mismatch")
        objc = ROOT / "src/Systems/Objective-C/pattern_sweep.m"
        headers = run(["gcc", "-print-file-name=include"], capture=True).strip()
        libobjc = run(["gcc", "-print-file-name=libobjc.so"], capture=True).strip()
        require(Path(libobjc).is_file(), f"Objective-C runtime library missing: {libobjc}")
        flags = run(["gnustep-config", "--objc-flags"], capture=True).split()
        libs = run(["gnustep-config", "--base-libs"], capture=True).split()
        run(["clang", *flags, "-Wall", "-Wextra", "-Werror", f"-I{headers}", str(objc), "-o", str(work / "objc"), f"-L{Path(libobjc).parent}", *libs])
        require(last_line(run([str(work / "objc")], capture=True)) == "Objective-C pattern sweep: 39/39 examples passed", "Objective-C aggregate output mismatch")
        nim = ROOT / "src/Niche/Nim/pattern_sweep.nim"
        run(["nim", "c", "--threads:on", "--hints:off", f"-o:{work / 'nim'}", str(nim)])
        require(last_line(run([str(work / "nim")], capture=True)) == "Nim pattern sweep: 39/39 examples passed", "Nim aggregate output mismatch")


def patterns_platform() -> None:
    profile = os.environ.get("GENKIDAMA_PLATFORM_PROFILE", "portable").lower()
    if profile == "matlab":
        run(["matlab", "-batch", "run('eng/ci/adapters/matlab_contract.m')"])
        return
    require(profile == "portable", f"unsupported platform profile: {profile}")
    run([sys.executable, "eng/ci/adapters/platform_source_contracts.py"])
    assembly_files = sorted((ROOT / "src/LowLevel/Assembly").glob("*.asm"))
    require(bool(assembly_files), "Assembly legacy contract inventory is empty")
    with tempfile.TemporaryDirectory(prefix="genkidama-assembly-") as temp:
        work = Path(temp)
        for index, source in enumerate(assembly_files):
            obj = work / f"cell-{index}.o"
            binary = work / f"cell-{index}"
            run(["nasm", "-f", "elf64", "-Wall", str(source), "-o", str(obj)])
            run(["ld", str(obj), "-o", str(binary)])
            run([str(binary)])
    godot = os.environ.get("GENKIDAMA_GODOT_BIN", "godot")
    output = run([godot, "--headless", "--script", str(ROOT / "src/Niche/GDScript/example1.gd")], capture=True)
    for marker in ["Dark Button", "Dark Checkbox", "Light Button", "Light Checkbox"]:
        require(marker in output.splitlines(), f"GDScript contract missing {marker}")
    micropython = os.environ.get("GENKIDAMA_MICROPYTHON_BIN", "/tmp/micropython/ports/unix/build-standard/micropython")
    output = run([micropython, str(ROOT / "src/Other/MicroPython/example1.py")], capture=True)
    for marker in ["Dark Button", "Dark Checkbox", "Light Button", "Light Checkbox"]:
        require(marker in output.splitlines(), f"MicroPython contract missing {marker}")
    rockstar = os.environ.get("GENKIDAMA_ROCKSTAR_BIN")
    require(bool(rockstar), "GENKIDAMA_ROCKSTAR_BIN is required")
    output = run([rockstar, str(ROOT / "src/Other/Rockstar/example1.rock")], capture=True)
    for marker in ["Dark Button", "Dark Checkbox", "Light Button", "Light Checkbox"]:
        require(marker in output.splitlines(), f"Rockstar contract missing {marker}")


CONTRACTS = {
    ("git", "learn"): learn_git,
    ("web", "learn"): learn_web,
    ("web", "patterns"): patterns_web,
    ("scripting", "learn"): learn_scripting,
    ("scripting", "patterns"): patterns_scripting,
    ("gnu", "learn"): learn_gnu,
    ("gnu", "patterns"): patterns_gnu,
    ("dart", "learn"): learn_dart,
    ("dart", "patterns"): patterns_dart,
    ("swift", "learn"): learn_swift,
    ("swift", "patterns"): patterns_swift,
    ("longtail", "patterns"): patterns_longtail,
    ("platform", "patterns"): patterns_platform,
}


def main() -> int:
    parser = argparse.ArgumentParser(description="Contracts extracted from retired language/pattern workflows.")
    parser.add_argument("family")
    parser.add_argument("surface", choices=["learn", "patterns"])
    args = parser.parse_args()
    contract = CONTRACTS.get((args.family, args.surface))
    if contract is None:
        raise ContractError(f"unsupported extracted contract: {args.family}/{args.surface}")
    contract()
    print(f"Extracted workflow contract: PASS family={args.family} surface={args.surface}", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ContractError as exc:
        print(f"Extracted workflow contract failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
