#!/usr/bin/env python3
from __future__ import annotations

import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
PROFILE = os.environ.get("GENKIDAMA_JVM_PROFILE", "").strip().lower()
EXPECTED_CELLS = 39


class ContractError(RuntimeError):
    pass


def run(argv: list[str], *, cwd: Path = ROOT, capture: bool = False) -> str:
    print(f"$ {' '.join(argv)}", flush=True)
    completed = subprocess.run(argv, cwd=cwd, text=True, check=False, stdout=subprocess.PIPE if capture else None, stderr=subprocess.STDOUT if capture else None)
    output = completed.stdout or ""
    if capture and output:
        print(output, end="" if output.endswith("\n") else "\n", flush=True)
    if completed.returncode != 0:
        raise ContractError(f"command failed with exit {completed.returncode}: {' '.join(argv)}")
    return output


def last_line(output: str) -> str:
    lines = [line.strip() for line in output.splitlines() if line.strip()]
    return lines[-1] if lines else ""


def validate_java_cells() -> None:
    files = sorted((ROOT / "src/Enterprise/Java/patterns").glob("*.java"))
    if len(files) != EXPECTED_CELLS:
        raise ContractError(f"Java pattern cell count is {len(files)}; expected {EXPECTED_CELLS}")
    with tempfile.TemporaryDirectory(prefix="genkidama-java-patterns-") as temp:
        work = Path(temp)
        for source in files:
            cell = work / "cell"
            shutil.rmtree(cell, ignore_errors=True)
            cell.mkdir()
            run(["javac", "-Xlint:all", "-Werror", "-d", str(cell), str(source)])
            run(["java", "-cp", str(cell), "PatternCell"])
    print(f"Java pattern cells: {EXPECTED_CELLS}/{EXPECTED_CELLS} passed", flush=True)


def validate_scala_sweep() -> None:
    pattern_files = sorted((ROOT / "src/Functional/Scala/patterns").glob("*.scala"))
    if len(pattern_files) != EXPECTED_CELLS:
        raise ContractError(f"Scala pattern cell count is {len(pattern_files)}; expected {EXPECTED_CELLS}")
    sources = [str(ROOT / "src/Functional/Scala/PatternSweep.scala"), *map(str, pattern_files)]
    output = run(["scala-cli", "run", *sources, "--server=false"], capture=True)
    if last_line(output) != "Scala pattern sweep: 39/39 examples passed":
        raise ContractError(f"Scala aggregate output mismatch: {last_line(output)!r}")


def validate_clojure_sweep() -> None:
    output = run(["clojure", "-M", str(ROOT / "src/Functional/Clojure/pattern_sweep.clj")], capture=True)
    if last_line(output) != "Clojure pattern sweep: 39/39 examples passed":
        raise ContractError(f"Clojure aggregate output mismatch: {last_line(output)!r}")


def validate_kotlin_cells() -> None:
    files = sorted((ROOT / "src/Enterprise/Kotlin/patterns").glob("*.kt"))
    if len(files) != EXPECTED_CELLS:
        raise ContractError(f"Kotlin pattern cell count is {len(files)}; expected {EXPECTED_CELLS}")
    sweep = ROOT / "src/Enterprise/Kotlin/PatternSweep.kt"
    with tempfile.TemporaryDirectory(prefix="genkidama-kotlin-patterns-") as temp:
        work = Path(temp)
        source_root = work / "src/main/kotlin"
        source_root.mkdir(parents=True)
        for source in files:
            shutil.copy2(source, source_root / source.name)
        shutil.copy2(sweep, source_root / sweep.name)
        (work / "settings.gradle.kts").write_text('rootProject.name = "genkidama-kotlin-patterns"\n', encoding="utf-8")
        (work / "build.gradle.kts").write_text('plugins {\n    kotlin("jvm") version "2.4.10"\n    application\n}\n\nrepositories { mavenCentral() }\n\nkotlin { jvmToolchain(17) }\n\napplication { mainClass.set("PatternSweepKt") }\n\ntasks.withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile>().configureEach {\n    compilerOptions.allWarningsAsErrors.set(true)\n}\n', encoding="utf-8")
        run(["gradle", "--no-daemon", "run"], cwd=work)
    print(f"Kotlin pattern cells: {EXPECTED_CELLS}/{EXPECTED_CELLS} passed", flush=True)


def validate_groovy_cells() -> None:
    files = sorted((ROOT / "src/Functional/Groovy/patterns").glob("*.groovy"))
    if len(files) != EXPECTED_CELLS:
        raise ContractError(f"Groovy pattern cell count is {len(files)}; expected {EXPECTED_CELLS}")
    for source in files:
        run(["groovy", str(source)])
    print(f"Groovy pattern cells: {EXPECTED_CELLS}/{EXPECTED_CELLS} passed", flush=True)


def main() -> int:
    run(["java", "--version"])
    if PROFILE == "java25":
        run(["javac", "-version"])
        validate_java_cells()
        validate_scala_sweep()
        validate_clojure_sweep()
        total = EXPECTED_CELLS * 3
    elif PROFILE == "jvm17":
        run(["gradle", "--version"])
        run(["groovy", "--version"])
        validate_kotlin_cells()
        validate_groovy_cells()
        total = EXPECTED_CELLS * 2
    else:
        raise ContractError("GENKIDAMA_JVM_PROFILE must be 'java25' or 'jvm17'")
    print(f"JVM Patterns contract: PASS profile={PROFILE} validations={total}", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ContractError as exc:
        print(f"JVM Patterns contract failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
