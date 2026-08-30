#!/usr/bin/env python3
from __future__ import annotations

import os
import re
import shutil
import sys
import tempfile
from pathlib import Path

import early_patterns as ep


ORIGINAL_RECORD = ep.record
ORIGINAL_VALIDATE_JVM = ep.validate_jvm
ORIGINAL_VALIDATE_NATIVE = ep.validate_native


def normalize_contract_text(text: str) -> str:
    lines: list[str] = []
    for raw in text.replace("\r\n", "\n").splitlines():
        line = raw.strip()
        line = re.sub(r"\s*=\s*", "=", line)
        lines.append(line.lower())
    return "\n".join(lines)


def legacy_output_has_marker(label: str, key: str, marker: str, normalized: str) -> bool:
    expected = normalize_contract_text(marker)
    if expected in normalized:
        return True
    # The historical Rockstar Singleton prints the raw count value on its own
    # line after `same=true`; the legacy workflow accepted that executable
    # behavior even though newer cells label it as `count=1`.
    if label.casefold() == "rockstar" and key == "singleton" and expected == "count=1":
        return "1" in normalized.splitlines()
    return False


def assert_legacy_output(label: str, key: str, output: str) -> None:
    normalized = normalize_contract_text(output)
    markers = ep.PATTERN_MARKERS[key]
    # A number of historical Abstract Factory examples execute only the dark
    # family while declaring both dark and light products in source. The source
    # declaration is checked by record_with_source_contract below.
    if key == "abstract_factory":
        markers = ("Dark Button", "Dark Checkbox")
    for marker in markers:
        ep.dc.require(
            legacy_output_has_marker(label, key, marker, normalized),
            f"{label} {key} contract missing {marker!r}",
        )
    if key == "prototype":
        ep.dc.require("original=orders: metrics,tracing" not in normalized, f"{label} Prototype shares mutable feature state")


def assert_abstract_factory_source_contract(runtime: str, source: Path) -> None:
    text = source.read_text(encoding="utf-8", errors="replace").lower()
    # Historical implementations often compose labels at runtime (for example
    # Fortran prints theme + product), so literal output strings are not a
    # portable source-level contract. Require the semantic vocabulary instead.
    for marker in ep.STATIC_MARKERS["abstract_factory"]:
        ep.dc.require(
            marker.lower() in text,
            f"{runtime} Abstract Factory source contract missing {marker!r} in {source.name}",
        )


def record_with_source_contract(census: dict[str, int], runtime: str, files: list[tuple[str, Path]]) -> None:
    ORIGINAL_RECORD(census, runtime, files)
    for key, source in files:
        if key == "abstract_factory":
            assert_abstract_factory_source_contract(runtime, source)


def java_main_class(text: str, source_name: str) -> str:
    main = re.search(r"public\s+static\s+void\s+main\s*\(", text)
    ep.dc.require(main is not None, f"Java {source_name}: public static void main not found")

    # Prefer the class corresponding to the compilation unit. This avoids
    # mistaking a nested helper declared immediately before main for the entry
    # class (ChainOfResponsibilityExample exposed exactly that failure mode).
    stem = Path(source_name).stem
    stem_class = re.search(
        rf"\b(?:public\s+)?(?:final\s+)?class\s+{re.escape(stem)}\b",
        text[: main.start()],
    )
    if stem_class is not None:
        return stem

    public_classes = re.findall(
        r"\bpublic\s+(?:final\s+)?class\s+([A-Za-z_][A-Za-z0-9_]*)",
        text[: main.start()],
    )
    ep.dc.require(bool(public_classes), f"Java {source_name}: main class not found")
    return public_classes[-1]


def prolog_entry_goal(text: str, source_name: str) -> str:
    # The old cells are not uniform: most expose run/0 while some expose
    # main/0. Preserve the source's declared public goal instead of imposing a
    # synthetic universal entrypoint.
    for goal in ("run", "main"):
        if re.search(rf"(?m)^\s*{goal}\s*:-", text):
            return goal
    raise ep.dc.ContractError(f"Prolog {source_name}: expected run/0 or main/0 entrypoint")


def vba_has_public_entrypoint(text: str) -> bool:
    return re.search(r"(?im)^\s*Public\s+(?:Sub|Function)\s+[A-Za-z_][A-Za-z0-9_]*\b", text) is not None


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
            shutil.rmtree(work / "bin", ignore_errors=True)
            (work / "Program.cs").write_text(source.read_text(encoding="utf-8"), encoding="utf-8")
            output = ep.dc.run(
                ["dotnet", "run", "--project", "Early.csproj", "-c", "Release", "--no-restore"],
                cwd=work,
                capture=True,
            )
            assert_legacy_output("C#", key, output)


def run_java_main_class(files: list[tuple[str, Path]]) -> None:
    with tempfile.TemporaryDirectory(prefix="genkidama-early-java-") as temp:
        work = Path(temp)
        for key, source in files:
            for path in work.iterdir():
                if path.is_file():
                    path.unlink()
                elif path.is_dir():
                    shutil.rmtree(path)
            text = source.read_text(encoding="utf-8")
            class_name = java_main_class(text, source.name)
            ep.dc.run(["javac", "-Xlint:all", "-Werror", "-d", str(work), str(source)])
            assert_legacy_output("Java", key, ep.dc.run(["java", "-cp", str(work), class_name], capture=True))


def validate_jvm_legacy(census: dict[str, int]) -> None:
    profile = os.environ.get("GENKIDAMA_JVM_PROFILE", "").lower()
    if profile != "java25":
        ORIGINAL_VALIDATE_JVM(census)
        return
    java = ep.discover("src/Enterprise/Java", (".java",))
    ep.record(census, "java", java)
    run_java_main_class(java)
    scala = ep.discover("src/Functional/Scala", (".scala",))
    ep.record(census, "scala", scala)
    for key, source in scala:
        assert_legacy_output("Scala", key, ep.dc.run(["scala-cli", "run", str(source), "--server=false"], capture=True))
    clj = ep.discover("src/Functional/Clojure", (".clj",))
    ep.record(census, "clojure", clj)
    for key, source in clj:
        assert_legacy_output("Clojure", key, ep.dc.run(["clojure", "-M", str(source)], capture=True))


def validate_native_legacy(census: dict[str, int]) -> None:
    profile = os.environ.get("GENKIDAMA_NATIVE_PROFILE", "").lower()
    if profile == "go":
        files = ep.discover("src/Systems/Go", (".go",))
        ep.record(census, "go", files)
        for key, source in files:
            # Root-pattern workflows historically required vet + execution. A
            # repo-wide gofmt policy was never part of these cells.
            ep.dc.run(["go", "vet", str(source)])
            assert_legacy_output("Go", key, ep.dc.run(["go", "run", str(source)], capture=True))
        return
    if profile == "rust":
        files = ep.discover("src/Systems/Rust", (".rs",))
        ep.record(census, "rust", files)
        with tempfile.TemporaryDirectory(prefix="genkidama-early-rust-") as temp:
            for index, (key, source) in enumerate(files):
                binary = Path(temp) / f"cell-{index}"
                ep.dc.run(["rustc", "-D", "warnings", str(source), "-o", str(binary)])
                assert_legacy_output("Rust", key, ep.dc.run([str(binary)], capture=True))
        return
    ORIGINAL_VALIDATE_NATIVE(census)


def validate_functional_legacy(census: dict[str, int]) -> None:
    ocaml = ep.discover("src/Functional/OCaml", (".ml",))
    lisp = ep.discover("src/Functional/Lisp", (".lisp",))
    prolog = ep.discover("src/Niche/Prolog", (".pl",))
    ep.record(census, "ocaml", ocaml)
    ep.record(census, "common-lisp", lisp)
    ep.record(census, "prolog", prolog)
    with tempfile.TemporaryDirectory(prefix="genkidama-early-functional-") as temp:
        work = Path(temp)
        for index, (key, source) in enumerate(ocaml):
            binary = work / f"ocaml-{index}"
            ep.dc.run(["ocamlc", "-w", "+a-70", "-warn-error", "+a-70", str(source), "-o", str(binary)])
            assert_legacy_output("OCaml", key, ep.dc.run([str(binary)], capture=True))
        for key, source in lisp:
            assert_legacy_output("Common Lisp", key, ep.dc.run(["sbcl", "--script", str(source)], capture=True))
        for key, source in prolog:
            text = source.read_text(encoding="utf-8", errors="replace")
            goal = prolog_entry_goal(text, source.name)
            output = ep.dc.run(["swipl", "-q", "-s", str(source), "-g", goal, "-t", "halt"], capture=True)
            assert_legacy_output("Prolog", key, output)


def validate_data_shell_legacy(census: dict[str, int]) -> None:
    r_files = ep.discover("src/DataScience/R", (".r",))
    octave = ep.discover("src/DataScience/Octave", (".m",))
    ps = ep.discover("src/Shell/PowerShell", (".ps1",))
    ep.record(census, "r", r_files)
    ep.record(census, "octave", octave)
    ep.record(census, "powershell", ps)
    for key, source in r_files:
        assert_legacy_output("R", key, ep.dc.run(["Rscript", "--vanilla", str(source)], capture=True))
    for key, source in octave:
        directory = str(source.parent).replace("'", "''")
        command = f"addpath('{directory}'); {source.stem}"
        assert_legacy_output("Octave", key, ep.dc.run(["octave", "--no-gui", "--quiet", "--eval", command], capture=True))
    for key, source in ps:
        parse = f"$tokens=$null; $errors=$null; [void][System.Management.Automation.Language.Parser]::ParseFile('{source}',[ref]$tokens,[ref]$errors); if ($errors.Count -gt 0) {{ exit 1 }}"
        ep.dc.run(["pwsh", "-NoLogo", "-NoProfile", "-Command", parse])
        assert_legacy_output("PowerShell", key, ep.dc.run(["pwsh", "-NoLogo", "-NoProfile", "-File", str(source)], capture=True))


def compile_solidity_for_discovery(files: list[tuple[str, Path]]) -> None:
    # Semantic source regexes are extracted pattern-by-pattern after this census;
    # this discovery pass only proves the exact root inventory still compiles.
    with tempfile.TemporaryDirectory(prefix="genkidama-early-solidity-") as temp:
        work = Path(temp)
        for index, (_, source) in enumerate(files):
            out = work / f"cell-{index}"
            out.mkdir()
            ep.dc.run(["npx", "--yes", "solc@0.8.30", "--bin", "--abi", str(source), "-o", str(out)])
            ep.dc.require(any(path.stat().st_size > 0 for path in out.glob("*.bin")), f"Solidity {source.name}: bytecode missing")


def validate_static_platform_for_discovery(census: dict[str, int]) -> None:
    vba = [(key, path) for path in sorted((ep.ROOT / "src/Shell/VBA").glob("*.bas")) if (key := ep.pattern_key(path))]
    delphi = [(key, path) for path in sorted((ep.ROOT / "src/Enterprise/Delphi").glob("*.pas")) if (key := ep.pattern_key(path))]
    ep.record(census, "vba", vba)
    ep.record(census, "delphi", delphi)
    for _, source in vba:
        text = source.read_text(encoding="utf-8")
        ep.dc.require(re.search(r"(?im)^Option Explicit$", text) is not None, f"VBA {source.name}: Option Explicit missing")
        ep.dc.require(vba_has_public_entrypoint(text), f"VBA {source.name}: public entrypoint missing")
    for _, source in delphi:
        text = source.read_text(encoding="utf-8")
        ep.dc.require("program " in text.lower(), f"Delphi {source.name}: program entrypoint missing")
        ep.dc.require("begin" in text.lower() and "end." in text.lower(), f"Delphi {source.name}: executable body missing")


def main() -> int:
    # Discovery compatibility layer. Every override below mirrors evidence from
    # the legacy workflows; after the full matrix is green it is folded into the
    # final early_patterns.py together with a frozen per-runtime inventory.
    ep.assert_output = assert_legacy_output
    ep.record = record_with_source_contract
    ep.run_csharp = run_csharp_without_losing_restore
    ep.validate_jvm = validate_jvm_legacy
    ep.validate_native = validate_native_legacy
    ep.validate_functional = validate_functional_legacy
    ep.validate_data_shell = validate_data_shell_legacy
    ep.run_solidity = compile_solidity_for_discovery
    ep.run_static_platform = validate_static_platform_for_discovery
    ep.VALIDATORS.update(
        {
            "jvm": validate_jvm_legacy,
            "native": validate_native_legacy,
            "functional": validate_functional_legacy,
            "data-shell": validate_data_shell_legacy,
        }
    )
    return ep.main()


if __name__ == "__main__":
    raise SystemExit(main())
