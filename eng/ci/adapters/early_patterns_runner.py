#!/usr/bin/env python3
from __future__ import annotations

import shutil
import sys
import tempfile
from pathlib import Path

import early_patterns as ep


def run_csharp_without_losing_restore(files: list[tuple[str, Path]]) -> None:
    project = """<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
  </PropertyGroup>
</Project>
"""
    with tempfile.TemporaryDirectory(prefix="genkidama-early-csharp-") as temp:
        work = Path(temp)
        (work / "Early.csproj").write_text(project, encoding="utf-8")
        (work / "Program.cs").write_text("System.Console.WriteLine();\n", encoding="utf-8")
        ep.dc.run(["dotnet", "restore", "Early.csproj"], cwd=work)
        for key, source in files:
            # obj/project.assets.json belongs to the restored project and must
            # survive across source substitutions when --no-restore is used.
            shutil.rmtree(work / "bin", ignore_errors=True)
            (work / "Program.cs").write_text(source.read_text(encoding="utf-8"), encoding="utf-8")
            output = ep.dc.run(
                ["dotnet", "run", "--project", "Early.csproj", "-c", "Release", "--no-restore"],
                cwd=work,
                capture=True,
            )
            ep.assert_output("C#", key, output)


def main() -> int:
    # Temporary compatibility shim for the discovery pass. Once the runtime
    # census is frozen this implementation is folded back into early_patterns.py.
    ep.run_csharp = run_csharp_without_losing_restore
    return ep.main()


if __name__ == "__main__":
    raise SystemExit(main())
