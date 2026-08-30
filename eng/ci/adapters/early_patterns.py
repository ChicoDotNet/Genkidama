#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path

import debt_contracts as dc

ROOT = dc.ROOT

PATTERN_MARKERS: dict[str, tuple[str, ...]] = {
    "abstract_factory": ("Dark Button", "Dark Checkbox", "Light Button", "Light Checkbox"),
    "adapter": ("legacy=86F", "adapted=30C"),
    "bridge": ("basic-tv=TV:on", "basic-radio=Radio:on", "mute-tv=TV:muted", "mute-radio=Radio:muted"),
    "builder": ("# Service status", "99.95%", "<h1>Service status</h1>"),
    "chain_of_responsibility": ("visited=faq>billing;handled=billing;result=refund(250)",),
    "composite": ("leaf=2", "docs=8", "root=10"),
    "decorator": ("base=alert", "audit=audit(alert)", "encrypted=enc(alert)", "stacked=audit(enc(alert))"),
    "facade": ("checkout=auth(alice)>reserve(SKU-42)>charge(499)",),
    "factory_method": ("PostgreSQL connect", "PostgreSQL query", "MySQL connect", "MySQL query"),
    "flyweight": ("styles=2;shared=true;text=ABC",),
    "prototype": ("original=orders: metrics", "clone=orders-canary: metrics,tracing"),
    "proxy": ("backend=1;fetches=1;first=doc(42);second=doc(42)",),
    "singleton": ("same=true", "count=1"),
}

STATIC_MARKERS: dict[str, tuple[str, ...]] = {
    "abstract_factory": ("factory", "dark", "light", "button", "checkbox"),
    "adapter": ("adapter", "fahrenheit", "celsius"),
    "bridge": ("bridge", "device", "remote", "mute"),
    "builder": ("builder", "service status", "99.95"),
    "chain_of_responsibility": ("handler", "billing", "refund"),
    "composite": ("composite", "size"),
    "decorator": ("decorator", "audit", "enc"),
    "facade": ("facade", "reserve", "charge"),
    "factory_method": ("postgresql", "mysql"),
    "flyweight": ("flyweight", "style", "shared"),
    "prototype": ("prototype", "clone", "tracing"),
    "proxy": ("proxy", "cache", "backend"),
    "singleton": ("singleton", "instance", "count"),
}

# First pass intentionally records the live census in CI. These values are frozen
# after the full matrix proves the runners; once populated, any lost/extra root
# contract becomes a hard failure before legacy YAML can be retired.
EXPECTED_COUNTS: dict[str, dict[str, int]] = {}


def pattern_key(path: Path) -> str | None:
    stem = path.stem.lower()
    compact = re.sub(r"[^a-z0-9]", "", stem)
    if compact == "example1":
        return "abstract_factory"
    compact = compact.replace("example", "")
    aliases = {
        "abstractfactory": "abstract_factory",
        "adapter": "adapter",
        "bridge": "bridge",
        "builder": "builder",
        "chainofresponsibility": "chain_of_responsibility",
        "composite": "composite",
        "decorator": "decorator",
        "facade": "facade",
        "factorymethod": "factory_method",
        "flyweight": "flyweight",
        "prototype": "prototype",
        "proxy": "proxy",
        "singleton": "singleton",
    }
    return aliases.get(compact)


def discover(directory: str, suffixes: tuple[str, ...]) -> list[tuple[str, Path]]:
    root = ROOT / directory
    dc.require(root.is_dir(), f"early-pattern directory missing: {directory}")
    result: list[tuple[str, Path]] = []
    for path in sorted(root.iterdir()):
        if not path.is_file() or path.suffix.lower() not in suffixes:
            continue
        key = pattern_key(path)
        if key is not None:
            result.append((key, path))
    return result


def assert_output(label: str, key: str, output: str) -> None:
    for marker in PATTERN_MARKERS[key]:
        dc.require(marker in output, f"{label} {key} contract missing {marker!r}")
    if key == "prototype":
        dc.require("original=orders: metrics,tracing" not in output, f"{label} Prototype shares mutable feature state")


def record(census: dict[str, int], runtime: str, files: list[tuple[str, Path]]) -> None:
    dc.require(files, f"{runtime} has no discoverable pre-CoR pattern contracts")
    keys = [key for key, _ in files]
    dc.require(len(keys) == len(set(keys)), f"{runtime} has duplicate pre-CoR pattern keys: {keys}")
    census[runtime] = len(files)
    print(f"EARLY_CELLS runtime={runtime} cells={len(files)} patterns={','.join(keys)}", flush=True)


def check_expected(family: str, census: dict[str, int]) -> None:
    expected = EXPECTED_COUNTS.get(family)
    if expected is None:
        print(f"EARLY_CENSUS family={family} discovered={json.dumps(census, sort_keys=True)}", flush=True)
        return
    dc.require(census == expected, f"{family} pre-CoR census changed: expected={expected} actual={census}")
    print(f"EARLY_CENSUS family={family} frozen={json.dumps(census, sort_keys=True)}", flush=True)


def run_csharp(files: list[tuple[str, Path]]) -> None:
    project = """<Project Sdk=\"Microsoft.NET.Sdk\">\n  <PropertyGroup>\n    <OutputType>Exe</OutputType>\n    <TargetFramework>net10.0</TargetFramework>\n    <ImplicitUsings>enable</ImplicitUsings>\n    <Nullable>enable</Nullable>\n    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>\n  </PropertyGroup>\n</Project>\n"""
    with tempfile.TemporaryDirectory(prefix="genkidama-early-csharp-") as temp:
        work = Path(temp)
        (work / "Early.csproj").write_text(project, encoding="utf-8")
        dc.run(["dotnet", "restore", "Early.csproj"], cwd=work)
        for key, source in files:
            shutil.rmtree(work / "bin", ignore_errors=True)
            shutil.rmtree(work / "obj", ignore_errors=True)
            (work / "Program.cs").write_text(source.read_text(encoding="utf-8"), encoding="utf-8")
            output = dc.run(["dotnet", "run", "--project", "Early.csproj", "-c", "Release", "--no-restore"], cwd=work, capture=True)
            assert_output("C#", key, output)


def run_vb(files: list[tuple[str, Path]]) -> None:
    project = """<Project Sdk=\"Microsoft.NET.Sdk\">\n  <PropertyGroup>\n    <OutputType>Exe</OutputType>\n    <TargetFramework>net10.0</TargetFramework>\n    <OptionExplicit>On</OptionExplicit>\n    <OptionInfer>On</OptionInfer>\n    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>\n  </PropertyGroup>\n</Project>\n"""
    with tempfile.TemporaryDirectory(prefix="genkidama-early-vb-") as temp:
        work = Path(temp)
        (work / "Early.vbproj").write_text(project, encoding="utf-8")
        (work / "Program.vb").write_text("Module Program\nSub Main()\nEnd Sub\nEnd Module\n", encoding="utf-8")
        dc.run(["dotnet", "restore", "Early.vbproj"], cwd=work)
        for key, source in files:
            shutil.rmtree(work / "bin", ignore_errors=True)
            (work / "Program.vb").write_text(source.read_text(encoding="utf-8"), encoding="utf-8")
            output = dc.run(["dotnet", "run", "--project", "Early.vbproj", "-c", "Release", "--no-restore"], cwd=work, capture=True)
            assert_output("VB.NET", key, output)


def run_java(files: list[tuple[str, Path]]) -> None:
    with tempfile.TemporaryDirectory(prefix="genkidama-early-java-") as temp:
        work = Path(temp)
        for key, source in files:
            for path in work.iterdir():
                if path.is_file(): path.unlink()
                elif path.is_dir(): shutil.rmtree(path)
            text = source.read_text(encoding="utf-8")
            match = re.search(r"\b(?:public\s+)?(?:final\s+)?class\s+([A-Za-z_][A-Za-z0-9_]*)", text)
            dc.require(match is not None, f"Java {source.name}: class name not found")
            class_name = match.group(1)
            dc.run(["javac", "-Xlint:all", "-Werror", "-d", str(work), str(source)])
            assert_output("Java", key, dc.run(["java", "-cp", str(work), class_name], capture=True))


def run_kotlin(files: list[tuple[str, Path]]) -> None:
    with tempfile.TemporaryDirectory(prefix="genkidama-early-kotlin-") as temp:
        work = Path(temp)
        for index, (key, source) in enumerate(files):
            jar = work / f"cell-{index}.jar"
            dc.run(["kotlinc", str(source), "-include-runtime", "-d", str(jar)])
            assert_output("Kotlin", key, dc.run(["java", "-jar", str(jar)], capture=True))


def compile_run(files: list[tuple[str, Path]], label: str, compiler) -> None:
    with tempfile.TemporaryDirectory(prefix=f"genkidama-early-{label.lower().replace(' ', '-')}-") as temp:
        work = Path(temp)
        for index, (key, source) in enumerate(files):
            binary = work / f"cell-{index}"
            argv = compiler(source, binary, work)
            dc.run(argv)
            assert_output(label, key, dc.run([str(binary)], capture=True))


def run_solidity(files: list[tuple[str, Path]]) -> None:
    with tempfile.TemporaryDirectory(prefix="genkidama-early-solidity-") as temp:
        work = Path(temp)
        for index, (key, source) in enumerate(files):
            out = work / f"cell-{index}"
            out.mkdir()
            dc.run(["npx", "--yes", "solc@0.8.30", "--bin", "--abi", str(source), "-o", str(out)])
            dc.require(any(path.stat().st_size > 0 for path in out.glob("*.bin")), f"Solidity {source.name}: bytecode missing")
            text = source.read_text(encoding="utf-8").lower()
            for marker in STATIC_MARKERS[key]:
                dc.require(marker.lower() in text, f"Solidity {key} source contract missing {marker!r}")


def run_static_platform(census: dict[str, int]) -> None:
    vba_dir = ROOT / "src/Shell/VBA"
    delphi_dir = ROOT / "src/Enterprise/Delphi"
    vba_main = [(key, path) for path in sorted(vba_dir.glob("*.bas")) if (key := pattern_key(path))]
    delphi_main = [(key, path) for path in sorted(delphi_dir.glob("*.pas")) if (key := pattern_key(path))]
    record(census, "vba", vba_main)
    record(census, "delphi", delphi_main)
    vba_corpus = "\n".join(path.read_text(encoding="utf-8") for path in sorted(vba_dir.iterdir()) if path.is_file()).lower()
    for key, source in vba_main:
        local = source.read_text(encoding="utf-8").lower()
        dc.require("option explicit" in local, f"VBA {source.name}: Option Explicit missing")
        for marker in STATIC_MARKERS[key]:
            dc.require(marker.lower() in vba_corpus, f"VBA {key} source contract missing {marker!r}")
    for key, source in delphi_main:
        text = source.read_text(encoding="utf-8").lower()
        for marker in STATIC_MARKERS[key]:
            dc.require(marker.lower() in text, f"Delphi {key} source contract missing {marker!r}")


def validate_dotnet(census: dict[str, int]) -> None:
    csharp = discover("src/Enterprise/C#", (".cs",)); record(census, "csharp", csharp); run_csharp(csharp)
    fsharp = discover("src/Functional/F#", (".fsx",)); record(census, "fsharp", fsharp)
    for key, source in fsharp: assert_output("F#", key, dc.run(["dotnet", "fsi", str(source)], capture=True))
    vb = discover("src/Enterprise/VisualBasic", (".vb",)); record(census, "vbnet", vb); run_vb(vb)


def validate_jvm(census: dict[str, int]) -> None:
    profile = os.environ.get("GENKIDAMA_JVM_PROFILE", "").lower()
    if profile == "java25":
        java = discover("src/Enterprise/Java", (".java",)); record(census, "java", java); run_java(java)
        scala = discover("src/Functional/Scala", (".scala",)); record(census, "scala", scala)
        for key, source in scala: assert_output("Scala", key, dc.run(["scala-cli", "run", str(source), "--server=false"], capture=True))
        clj = discover("src/Functional/Clojure", (".clj",)); record(census, "clojure", clj)
        for key, source in clj: assert_output("Clojure", key, dc.run(["clojure", "-M", str(source)], capture=True))
    elif profile == "jvm17":
        kotlin = discover("src/Enterprise/Kotlin", (".kt",)); record(census, "kotlin", kotlin); run_kotlin(kotlin)
        groovy = discover("src/Scripting/Groovy", (".groovy",)); record(census, "groovy", groovy)
        for key, source in groovy: assert_output("Groovy", key, dc.run(["groovy", str(source)], capture=True))
    else:
        raise dc.ContractError(f"unsupported JVM early-pattern profile: {profile}")


def validate_native(census: dict[str, int]) -> None:
    profile = os.environ.get("GENKIDAMA_NATIVE_PROFILE", "").lower()
    if profile == "gnu":
        c = discover("src/Systems/C", (".c",)); record(census, "c", c)
        compile_run(c, "C", lambda s, b, w: ["gcc-14", "-std=c23", "-Wall", "-Wextra", "-Werror", str(s), "-o", str(b)])
        cpp = discover("src/Systems/C++", (".cpp",)); record(census, "cpp", cpp)
        compile_run(cpp, "C++", lambda s, b, w: ["g++-14", "-std=c++23", "-Wall", "-Wextra", "-Werror", str(s), "-o", str(b)])
    elif profile == "rust":
        rust = discover("src/Systems/Rust", (".rs",)); record(census, "rust", rust)
        with tempfile.TemporaryDirectory(prefix="genkidama-early-rust-") as temp:
            for index, (key, source) in enumerate(rust):
                binary = Path(temp) / f"cell-{index}"
                dc.run(["rustfmt", "--check", str(source)])
                dc.run(["rustc", "--edition=2024", "-D", "warnings", str(source), "-o", str(binary)])
                assert_output("Rust", key, dc.run([str(binary)], capture=True))
    elif profile == "go":
        go = discover("src/Systems/Go", (".go",)); record(census, "go", go)
        for key, source in go:
            dc.require(not dc.run(["gofmt", "-d", str(source)], capture=True).strip(), f"Go {source.name} is not gofmt-clean")
            dc.run(["go", "vet", str(source)])
            assert_output("Go", key, dc.run(["go", "run", str(source)], capture=True))
    else:
        raise dc.ContractError(f"unsupported Native early-pattern profile: {profile}")


def validate_beam(census: dict[str, int]) -> None:
    elixir = discover("src/Functional/Elixir", (".exs",)); record(census, "elixir", elixir)
    erlang = discover("src/Functional/Erlang", (".erl",)); record(census, "erlang", erlang)
    with tempfile.TemporaryDirectory(prefix="genkidama-early-beam-") as temp:
        work = Path(temp)
        for key, source in elixir:
            out = work / "elixir"; shutil.rmtree(out, ignore_errors=True); out.mkdir()
            dc.run(["elixirc", "--warnings-as-errors", "-o", str(out), str(source)])
            assert_output("Elixir", key, dc.run(["elixir", str(source)], capture=True))
        for key, source in erlang:
            out = work / "erlang"; shutil.rmtree(out, ignore_errors=True); out.mkdir()
            text = source.read_text(encoding="utf-8")
            match = re.search(r"-module\(([^)]+)\)\.", text)
            dc.require(match is not None, f"Erlang {source.name}: module declaration missing")
            module = match.group(1)
            dc.run(["erlc", "-Werror", "-o", str(out), str(source)])
            output = dc.run(["erl", "-noshell", "-pa", str(out), "-eval", f"{module}:main(), halt()."], capture=True)
            assert_output("Erlang", key, output)


def validate_functional(census: dict[str, int]) -> None:
    ocaml = discover("src/Functional/OCaml", (".ml",)); record(census, "ocaml", ocaml)
    lisp = discover("src/Functional/Lisp", (".lisp",)); record(census, "common-lisp", lisp)
    prolog = discover("src/Niche/Prolog", (".pl",)); record(census, "prolog", prolog)
    with tempfile.TemporaryDirectory(prefix="genkidama-early-functional-") as temp:
        work = Path(temp)
        for index, (key, source) in enumerate(ocaml):
            binary = work / f"ocaml-{index}"
            dc.run(["ocamlc", "-w", "+a-70", "-warn-error", "+a-70", str(source), "-o", str(binary)])
            assert_output("OCaml", key, dc.run([str(binary)], capture=True))
        for key, source in lisp: assert_output("Common Lisp", key, dc.run(["sbcl", "--script", str(source)], capture=True))
        for key, source in prolog: assert_output("Prolog", key, dc.run(["swipl", "-q", "-f", str(source)], capture=True))


def validate_data_shell(census: dict[str, int]) -> None:
    r = discover("src/DataScience/R", (".r",)); record(census, "r", r)
    octave = discover("src/DataScience/Octave", (".m",)); record(census, "octave", octave)
    ps = discover("src/Shell/PowerShell", (".ps1",)); record(census, "powershell", ps)
    for key, source in r: assert_output("R", key, dc.run(["Rscript", "--vanilla", str(source)], capture=True))
    for key, source in octave: assert_output("Octave", key, dc.run(["octave", "--no-gui", "--quiet", str(source)], capture=True))
    for key, source in ps:
        parse = f"$tokens=$null; $errors=$null; [void][System.Management.Automation.Language.Parser]::ParseFile('{source}',[ref]$tokens,[ref]$errors); if ($errors.Count -gt 0) {{ exit 1 }}"
        dc.run(["pwsh", "-NoLogo", "-NoProfile", "-Command", parse])
        assert_output("PowerShell", key, dc.run(["pwsh", "-NoLogo", "-NoProfile", "-File", str(source)], capture=True))


def validate_web(census: dict[str, int]) -> None:
    js = discover("src/Web/JavaScriptJS", (".js",)); record(census, "javascript", js)
    ts = discover("src/Web/TypeScriptTS", (".ts",)); record(census, "typescript", ts)
    sol = discover("src/Niche/Solidity", (".sol",)); record(census, "solidity", sol)
    for key, source in js:
        dc.run(["node", "--check", str(source)])
        assert_output("JavaScript", key, dc.run(["node", str(source)], capture=True))
    with tempfile.TemporaryDirectory(prefix="genkidama-early-ts-") as temp:
        work = Path(temp)
        for index, (key, source) in enumerate(ts):
            out = work / f"cell-{index}"; out.mkdir()
            dc.run(["npx", "--yes", "--package", "typescript@6.0.3", "tsc", str(source), "--strict", "--target", "ES2024", "--module", "commonjs", "--outDir", str(out)])
            assert_output("TypeScript", key, dc.run(["node", str(out / f"{source.stem}.js")], capture=True))
    run_solidity(sol)


def validate_scripting(census: dict[str, int]) -> None:
    profile = os.environ.get("GENKIDAMA_SCRIPTING_PROFILE", "linux").lower()
    dc.require(profile in {"linux", "windows"}, f"unsupported scripting early-pattern profile: {profile}")
    if profile == "windows":
        print("EARLY_CENSUS family=scripting profile=windows no-pattern-surface", flush=True)
        return
    specs = [
        ("python", "src/Scripting/PythonPY", (".py",)),
        ("php", "src/Scripting/PHP", (".php",)),
        ("ruby", "src/Scripting/RubyRB", (".rb",)),
        ("lua", "src/Scripting/Lua", (".lua",)),
        ("bash", "src/Shell/Bash", (".sh",)),
        ("perl", "src/Scripting/Perl", (".pl",)),
    ]
    for runtime, directory, suffixes in specs:
        files = discover(directory, suffixes); record(census, runtime, files)
        for key, source in files:
            if runtime == "python":
                dc.run([sys.executable, "-m", "py_compile", str(source)]); output = dc.run([sys.executable, str(source)], capture=True)
            elif runtime == "php":
                dc.run(["php", "-l", str(source)]); output = dc.run(["php", str(source)], capture=True)
            elif runtime == "ruby":
                dc.run(["ruby", "-c", str(source)]); output = dc.run(["ruby", str(source)], capture=True)
            elif runtime == "lua":
                luac = os.environ.get("GENKIDAMA_LUAC_BIN", "luac"); lua = os.environ.get("GENKIDAMA_LUA_BIN", "lua")
                dc.run([luac, "-p", str(source)]); output = dc.run([lua, str(source)], capture=True)
            elif runtime == "bash":
                bash = os.environ.get("GENKIDAMA_BASH_BIN", "bash"); dc.run([bash, "-n", str(source)]); output = dc.run([bash, str(source)], capture=True)
            else:
                dc.run(["perl", "-c", str(source)]); output = dc.run(["perl", str(source)], capture=True)
            assert_output(runtime, key, output)


def validate_gnu(census: dict[str, int]) -> None:
    ada = discover("src/Historical/Ada", (".adb",)); record(census, "ada", ada)
    pascal = discover("src/Historical/Pascal", (".pas",)); record(census, "pascal", pascal)
    cobol = discover("src/Historical/Cobol", (".cbl",)); record(census, "cobol", cobol)
    fortran = discover("src/Historical/Fortran", (".f90",)); record(census, "fortran", fortran)
    compile_run(ada, "Ada", lambda s, b, w: ["gnatmake", "-q", "-gnat2022", "-gnatwa", "-gnatwe", "-D", str(w), str(s), "-o", str(b)])
    compile_run(pascal, "Pascal", lambda s, b, w: ["fpc", "-O2", "-S2", "-vw", f"-FE{w}", f"-FU{w}", f"-o{b}", str(s)])
    compile_run(cobol, "COBOL", lambda s, b, w: ["cobc", "-x", "-Wall", str(s), "-o", str(b)])
    compile_run(fortran, "Fortran", lambda s, b, w: ["gfortran", "-std=f2018", "-Wall", "-Wextra", "-Werror", str(s), "-o", str(b)])


def validate_dart(census: dict[str, int]) -> None:
    files = discover("src/Web/Dart", (".dart",)); record(census, "dart", files)
    for key, source in files:
        dc.run(["dart", "format", "--output=none", "--set-exit-if-changed", str(source)])
        dc.run(["dart", "analyze", "--fatal-infos", "--fatal-warnings", str(source)])
        assert_output("Dart", key, dc.run(["dart", "run", str(source)], capture=True))


def validate_swift(census: dict[str, int]) -> None:
    files = discover("src/Systems/Swift", (".swift",)); record(census, "swift", files)
    compile_run(files, "Swift", lambda s, b, w: ["swiftc", str(s), "-o", str(b)])


def validate_longtail(census: dict[str, int]) -> None:
    specs = [
        ("haskell", "src/Functional/Haskell", (".hs",)),
        ("crystal", "src/Niche/Crystal", (".cr",)),
        ("zig", "src/Systems/Zig", (".zig",)),
        ("julia", "src/DataScience/Julia", (".jl",)),
        ("objective-c", "src/Systems/Objective-C", (".m",)),
        ("nim", "src/Niche/Nim", (".nim",)),
    ]
    all_files: dict[str, list[tuple[str, Path]]] = {}
    for runtime, directory, suffixes in specs:
        files = discover(directory, suffixes); record(census, runtime, files); all_files[runtime] = files
    for key, source in all_files["haskell"]: assert_output("Haskell", key, dc.run(["runghc", str(source)], capture=True))
    for key, source in all_files["crystal"]:
        dc.run(["crystal", "tool", "format", "--check", str(source)])
        assert_output("Crystal", key, dc.run(["crystal", "run", "--error-on-warnings", str(source)], capture=True))
    for key, source in all_files["zig"]:
        dc.run(["zig", "fmt", "--check", str(source)])
        assert_output("Zig", key, dc.run(["zig", "run", str(source)], capture=True))
    for key, source in all_files["julia"]: assert_output("Julia", key, dc.run(["julia", "--startup-file=no", "--check-bounds=yes", str(source)], capture=True))
    headers = dc.run(["gcc", "-print-file-name=include"], capture=True).strip()
    libobjc = dc.run(["gcc", "-print-file-name=libobjc.so"], capture=True).strip()
    flags = shlex.split(dc.run(["gnustep-config", "--objc-flags"], capture=True))
    libs = shlex.split(dc.run(["gnustep-config", "--base-libs"], capture=True))
    with tempfile.TemporaryDirectory(prefix="genkidama-early-objc-") as temp:
        for index, (key, source) in enumerate(all_files["objective-c"]):
            binary = Path(temp) / f"cell-{index}"
            dc.run(["clang", *flags, "-Wall", "-Wextra", "-Werror", f"-I{headers}", str(source), "-o", str(binary), f"-L{Path(libobjc).parent}", *libs])
            assert_output("Objective-C", key, dc.run([str(binary)], capture=True))
    with tempfile.TemporaryDirectory(prefix="genkidama-early-nim-") as temp:
        for index, (key, source) in enumerate(all_files["nim"]):
            binary = Path(temp) / f"cell-{index}"
            dc.run(["nim", "c", "--threads:on", "--hints:off", "--warnings:on", f"-o:{binary}", str(source)])
            assert_output("Nim", key, dc.run([str(binary)], capture=True))


def validate_platform(census: dict[str, int]) -> None:
    profile = os.environ.get("GENKIDAMA_PLATFORM_PROFILE", "portable").lower()
    if profile == "matlab":
        print("EARLY_CENSUS family=platform runtime=matlab delegated=matlab_contract.m", flush=True)
        return
    dc.require(profile == "portable", f"unsupported platform early-pattern profile: {profile}")
    run_static_platform(census)
    gd = discover("src/Niche/GDScript", (".gd",)); record(census, "gdscript", gd)
    mp = discover("src/Other/MicroPython", (".py",)); record(census, "micropython", mp)
    rock = discover("src/Other/Rockstar", (".rock",)); record(census, "rockstar", rock)
    godot = os.environ.get("GENKIDAMA_GODOT_BIN", "godot")
    micropython = os.environ.get("GENKIDAMA_MICROPYTHON_BIN", "/tmp/micropython/ports/unix/build-standard/micropython")
    rockstar = os.environ.get("GENKIDAMA_ROCKSTAR_BIN"); dc.require(bool(rockstar), "GENKIDAMA_ROCKSTAR_BIN is required")
    for key, source in gd: assert_output("GDScript", key, dc.run([godot, "--headless", "--script", str(source)], capture=True))
    for key, source in mp: assert_output("MicroPython", key, dc.run([micropython, str(source)], capture=True))
    for key, source in rock: assert_output("Rockstar", key, dc.run([rockstar, str(source)], capture=True))


VALIDATORS = {
    "dotnet": validate_dotnet,
    "jvm": validate_jvm,
    "native": validate_native,
    "beam": validate_beam,
    "functional": validate_functional,
    "data-shell": validate_data_shell,
    "web": validate_web,
    "scripting": validate_scripting,
    "gnu": validate_gnu,
    "dart": validate_dart,
    "swift": validate_swift,
    "longtail": validate_longtail,
    "platform": validate_platform,
}


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Validate pre-CoR/root pattern contracts inside an already-provisioned runtime family.")
    parser.add_argument("family", choices=sorted(VALIDATORS))
    args = parser.parse_args(argv)
    started = time.perf_counter()
    census: dict[str, int] = {}
    VALIDATORS[args.family](census)
    if census:
        check_expected(args.family, census)
    print(f"Early Patterns contract: PASS family={args.family} total_seconds={time.perf_counter() - started:.3f}", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except dc.ContractError as exc:
        print(f"Early Patterns contract failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
