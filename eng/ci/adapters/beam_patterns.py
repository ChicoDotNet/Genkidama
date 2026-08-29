#!/usr/bin/env python3
from __future__ import annotations

import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
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


def main() -> int:
    elixir_files = exact_files(ROOT / "src/Functional/Elixir/patterns", ".exs", "Elixir")
    erlang_files = exact_files(ROOT / "src/Functional/Erlang/patterns", ".erl", "Erlang")

    elixir_names = {path.stem for path in elixir_files}
    erlang_names = {path.stem for path in erlang_files}
    if elixir_names != erlang_names:
        only_elixir = sorted(elixir_names - erlang_names)
        only_erlang = sorted(erlang_names - elixir_names)
        raise ContractError(
            f"BEAM pattern census mismatch: only_elixir={only_elixir} only_erlang={only_erlang}"
        )

    run(["elixir", "--version"])
    run(["erlc", "-version"], capture=True)

    with tempfile.TemporaryDirectory(prefix="genkidama-beam-patterns-") as temp:
        work = Path(temp)
        elixir_out = work / "elixir"
        erlang_out = work / "erlang"
        elixir_out.mkdir()
        erlang_out.mkdir()

        for source in elixir_files:
            for compiled in elixir_out.glob("*.beam"):
                compiled.unlink()
            run(["elixirc", "--warnings-as-errors", "-o", str(elixir_out), str(source)])
            run(["elixir", "--warnings-as-errors", str(source)])
            print(f"PASS Elixir {source.name}", flush=True)

        for source in erlang_files:
            for compiled in erlang_out.glob("*.beam"):
                compiled.unlink()
            module = source.stem
            run(["erlc", "-Werror", "-o", str(erlang_out), str(source)])
            run([
                "erl",
                "-noshell",
                "-pa",
                str(erlang_out),
                "-eval",
                f"{module}:main(), halt().",
            ])
            print(f"PASS Erlang {source.name}", flush=True)

    print(f"Elixir pattern cells: {EXPECTED}/{EXPECTED} passed", flush=True)
    print(f"Erlang pattern cells: {EXPECTED}/{EXPECTED} passed", flush=True)
    print(f"BEAM Patterns contract: PASS validations={EXPECTED * 2}", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ContractError as exc:
        print(f"BEAM Patterns contract failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
