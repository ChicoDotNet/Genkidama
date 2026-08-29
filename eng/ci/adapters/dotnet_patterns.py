#!/usr/bin/env python3
from __future__ import annotations

import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
EXPECTED_CELLS = 39

VB_CONTRACTS: dict[str, tuple[str, ...]] = {
    "AdapterExample.vb": ("legacy=86F", "adapted=30C"),
    "BridgeExample.vb": ("basic-tv=TV:on", "basic-radio=Radio:on", "mute-tv=TV:muted", "mute-radio=Radio:muted"),
    "BuilderExample.vb": ("# Service status", "## Availability", "99.95%", "<h1>Service status</h1>", "<h2>Availability</h2>", "<p>99.95%</p>"),
    "CompositeExample.vb": ("leaf=2", "docs=8", "root=10"),
    "DecoratorExample.vb": ("base=alert", "audit=audit(alert)", "encrypted=enc(alert)", "stacked=audit(enc(alert))"),
    "Example1.vb": ("Dark Button", "Dark Checkbox", "Light Button", "Light Checkbox"),
    "Example2.vb": ("Connecting to PostgreSQL", "Querying PostgreSQL", "Connecting to MySQL", "Querying MySQL"),
    "Example3.vb": ("Generating PDF report", "Generating HTML report"),
    "FactoryMethodExample.vb": ("PostgreSQL connect", "PostgreSQL query", "MySQL connect", "MySQL query"),
    "PrototypeExample.vb": ("original=orders: metrics", "clone=orders-canary: metrics,tracing"),
    "SingletonExample.vb": ("same=true", "count=1"),
}


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


def validate_csharp_cells() -> None:
    source_dir = ROOT / "src/Enterprise/C#/patterns"
    files = sorted(source_dir.glob("*.cs"))
    if len(files) != EXPECTED_CELLS:
        raise ContractError(f"C# pattern cell count is {len(files)}; expected {EXPECTED_CELLS}")

    csproj = """<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <ImplicitUsings>disable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
  </PropertyGroup>
</Project>
"""
    program = """using System;
using System.Linq;
using System.Reflection;

var methods = Assembly.GetExecutingAssembly().GetTypes()
    .Where(t => t.Namespace == "Genkidama.PatternExamples")
    .Select(t => (Type: t, Method: t.GetMethod("Run", BindingFlags.Public | BindingFlags.Static, null, Type.EmptyTypes, null)))
    .Where(x => x.Method is not null)
    .OrderBy(x => x.Type.Name)
    .ToList();
if (methods.Count != __EXPECTED__) { Console.Error.WriteLine($"discovered={methods.Count} expected=__EXPECTED__"); return 2; }
foreach (var cell in methods)
{
    object? value;
    try { value = cell.Method!.Invoke(null, null); }
    catch (TargetInvocationException ex) { Console.Error.WriteLine($"{cell.Type.Name}: {ex.InnerException ?? ex}"); return 3; }
    if (value is not bool ok || !ok) { Console.Error.WriteLine($"{cell.Type.Name}: Run() returned false"); return 4; }
    Console.WriteLine($"PASS {cell.Type.Name}");
}
Console.WriteLine($"C# pattern cells: {methods.Count}/{methods.Count} passed");
return 0;
""".replace("__EXPECTED__", str(EXPECTED_CELLS))

    with tempfile.TemporaryDirectory(prefix="genkidama-csharp-patterns-") as temp:
        work = Path(temp)
        for source in files:
            shutil.copy2(source, work / source.name)
        (work / "Patterns.csproj").write_text(csproj, encoding="utf-8")
        (work / "Program.cs").write_text(program, encoding="utf-8")
        run(["dotnet", "build", "Patterns.csproj", "-c", "Release"], cwd=work)
        run(["dotnet", "run", "--project", "Patterns.csproj", "-c", "Release", "--no-build"], cwd=work)


def validate_fsharp_cells() -> None:
    source_dir = ROOT / "src/Functional/F#/patterns"
    files = sorted(source_dir.glob("*.fsx"))
    if len(files) != EXPECTED_CELLS:
        raise ContractError(f"F# pattern cell count is {len(files)}; expected {EXPECTED_CELLS}")

    module_pattern = re.compile(r"^\s*module\s+([A-Za-z_][A-Za-z0-9_']*)\s*$", re.MULTILINE)
    modules: list[tuple[str, str]] = []

    with tempfile.TemporaryDirectory(prefix="genkidama-fsharp-patterns-") as temp:
        work = Path(temp)
        compile_items: list[str] = []
        for index, source in enumerate(files):
            text = source.read_text(encoding="utf-8")
            match = module_pattern.search(text)
            if not match:
                raise ContractError(f"{source.name}: expected a top-level 'module Name' declaration")
            module = match.group(1)
            target_name = f"Cell{index:02d}_{source.stem}.fs"
            (work / target_name).write_text(text, encoding="utf-8")
            compile_items.append(target_name)
            modules.append((module, source.stem))

        program_lines = ["module Program", "", "[<EntryPoint>]", "let main _ ="]
        for module, label in modules:
            program_lines.extend([
                f"    if not ({module}.run ()) then failwith \"{label}: run returned false\"",
                f"    printfn \"PASS {label}\"",
            ])
        program_lines.extend([
            f"    printfn \"F# pattern cells: {EXPECTED_CELLS}/{EXPECTED_CELLS} passed\"",
            "    0",
            "",
        ])
        (work / "Program.fs").write_text("\n".join(program_lines), encoding="utf-8")

        includes = "\n".join(f'    <Compile Include="{name}" />' for name in compile_items + ["Program.fs"])
        fsproj = f"""<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
  </PropertyGroup>
  <ItemGroup>
{includes}
  </ItemGroup>
</Project>
"""
        (work / "Patterns.fsproj").write_text(fsproj, encoding="utf-8")
        run(["dotnet", "build", "Patterns.fsproj", "-c", "Release"], cwd=work)
        run(["dotnet", "run", "--project", "Patterns.fsproj", "-c", "Release", "--no-build"], cwd=work)


def validate_vb_examples() -> None:
    source_dir = ROOT / "src/Enterprise/VisualBasic"
    files = {path.name: path for path in source_dir.glob("*.vb")}
    expected = set(VB_CONTRACTS)
    if set(files) != expected:
        missing = sorted(expected - set(files))
        extra = sorted(set(files) - expected)
        raise ContractError(f"VB example inventory changed; missing={missing} extra={extra}")

    project = """<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <OptionExplicit>On</OptionExplicit>
    <OptionInfer>On</OptionInfer>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
  </PropertyGroup>
</Project>
"""

    with tempfile.TemporaryDirectory(prefix="genkidama-vb-patterns-") as temp:
        work = Path(temp)
        (work / "Pattern.vbproj").write_text(project, encoding="utf-8")
        (work / "Program.vb").write_text("Module Program\nSub Main()\nEnd Sub\nEnd Module\n", encoding="utf-8")
        run(["dotnet", "restore", "Pattern.vbproj"], cwd=work)

        for name, markers in VB_CONTRACTS.items():
            shutil.rmtree(work / "bin", ignore_errors=True)
            (work / "Program.vb").write_text(files[name].read_text(encoding="utf-8"), encoding="utf-8")
            completed = run(
                ["dotnet", "run", "--project", "Pattern.vbproj", "-c", "Release", "--no-restore"],
                cwd=work,
                capture=True,
            )
            output = completed.stdout
            for marker in markers:
                if marker not in output:
                    raise ContractError(f"{name}: missing output marker {marker!r}")
            if name == "PrototypeExample.vb" and "original=orders: metrics,tracing" in output:
                raise ContractError("PrototypeExample.vb: clone shares mutable Features state")
            print(f"PASS {name}", flush=True)

    print(f"VB.NET named pattern examples: {len(VB_CONTRACTS)}/{len(VB_CONTRACTS)} passed", flush=True)


def main() -> int:
    run(["dotnet", "--info"])
    validate_csharp_cells()
    validate_fsharp_cells()
    validate_vb_examples()
    print(".NET Patterns contract: PASS (78 post-CoR cells + 11 VB named examples)", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ContractError as exc:
        print(f".NET Patterns contract failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
