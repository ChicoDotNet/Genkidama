#!/usr/bin/env python3
from __future__ import annotations

import os
import shutil
import subprocess
import sys
import tarfile
import urllib.request
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
EXPECTED = 39
EXPECTED_OCAML = "5.5.0"
EXPECTED_SBCL = "SBCL 2.6.8"
EXPECTED_SWIPL_PREFIX = "SWI-Prolog version 9.0.4"
SBCL_VERSION = "2.6.8"
SBCL_URL = (
    "https://downloads.sourceforge.net/project/sbcl/sbcl/"
    f"{SBCL_VERSION}/sbcl-{SBCL_VERSION}-x86-64-linux-binary.tar.bz2"
)


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
    if capture and completed.stdout:
        print(completed.stdout, end="" if completed.stdout.endswith("\n") else "\n", flush=True)
    return completed.stdout or ""


def exact_files(path: Path, suffix: str, label: str) -> list[Path]:
    files = sorted(path.glob(f"*{suffix}"))
    if len(files) != EXPECTED:
        raise ContractError(f"{label} pattern cell count is {len(files)}; expected {EXPECTED}")
    return files


def require_exact(label: str, actual: str, expected: str) -> None:
    value = actual.strip()
    if value != expected:
        raise ContractError(f"{label} version is {value!r}; expected {expected!r}")


def require_prefix(label: str, actual: str, expected_prefix: str) -> None:
    value = actual.strip()
    if not value.startswith(expected_prefix):
        raise ContractError(f"{label} version is {value!r}; expected prefix {expected_prefix!r}")


def ensure_stable_sbcl() -> list[str]:
    configured = os.environ.get("GENKIDAMA_SBCL_BIN")
    if configured:
        command = [configured]
    else:
        runner_temp = Path(os.environ.get("RUNNER_TEMP", "/tmp"))
        archive = runner_temp / f"sbcl-{SBCL_VERSION}-x86-64-linux-binary.tar.bz2"
        extracted = runner_temp / f"sbcl-{SBCL_VERSION}-x86-64-linux"
        launcher = extracted / "run-sbcl.sh"
        if not launcher.is_file():
            archive.unlink(missing_ok=True)
            if extracted.exists():
                shutil.rmtree(extracted)
            print(f"Downloading stable SBCL {SBCL_VERSION} from SourceForge", flush=True)
            urllib.request.urlretrieve(SBCL_URL, archive)
            with tarfile.open(archive, "r:bz2") as tar:
                tar.extractall(runner_temp, filter="data")
        if not launcher.is_file():
            raise ContractError(f"SBCL {SBCL_VERSION} launcher missing after extraction: {launcher}")
        command = [str(launcher)]

    require_exact("SBCL", run([*command, "--version"], capture=True), EXPECTED_SBCL)
    return command


def main() -> int:
    ocaml_files = exact_files(ROOT / "src/Functional/OCaml/patterns", ".ml", "OCaml")
    lisp_files = exact_files(ROOT / "src/Functional/CommonLisp/patterns", ".lisp", "Common Lisp")
    prolog_files = exact_files(ROOT / "src/Functional/Prolog/patterns", ".pl", "Prolog")

    stem_sets = {
        "OCaml": {path.stem for path in ocaml_files},
        "Common Lisp": {path.stem for path in lisp_files},
        "Prolog": {path.stem for path in prolog_files},
    }
    canonical = stem_sets["OCaml"]
    for label, stems in stem_sets.items():
        if stems != canonical:
            missing = sorted(canonical - stems)
            extra = sorted(stems - canonical)
            raise ContractError(
                f"Functional pattern census mismatch for {label}: missing={missing} extra={extra}"
            )

    require_exact("OCaml", run(["ocamlc", "-version"], capture=True), EXPECTED_OCAML)
    sbcl = ensure_stable_sbcl()
    require_prefix("SWI-Prolog", run(["swipl", "--version"], capture=True), EXPECTED_SWIPL_PREFIX)

    with tempfile.TemporaryDirectory(prefix="genkidama-functional-patterns-") as temp:
        work = Path(temp)
        ocaml_work = work / "ocaml"
        ocaml_work.mkdir()

        for source in ocaml_files:
            for generated in ocaml_work.iterdir():
                if generated.is_file():
                    generated.unlink()
            cell = ocaml_work / "cell.ml"
            shutil.copyfile(source, cell)
            run(
                [
                    "ocamlc",
                    "-w",
                    "+a-70",
                    "-warn-error",
                    "+a-70",
                    "cell.ml",
                    "-o",
                    "cell",
                ],
                cwd=ocaml_work,
            )
            run([str(ocaml_work / "cell")], cwd=ocaml_work)
            print(f"PASS OCaml {source.name}", flush=True)

        for source in lisp_files:
            run([*sbcl, "--script", str(source)])
            print(f"PASS Common Lisp {source.name}", flush=True)

        for source in prolog_files:
            run(["swipl", "-q", "-f", str(source)])
            print(f"PASS Prolog {source.name}", flush=True)

    print(f"OCaml pattern cells: {EXPECTED}/{EXPECTED} passed", flush=True)
    print(f"Common Lisp pattern cells: {EXPECTED}/{EXPECTED} passed", flush=True)
    print(f"Prolog pattern cells: {EXPECTED}/{EXPECTED} passed", flush=True)
    print(f"Functional Patterns contract: PASS validations={EXPECTED * 3}", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ContractError as exc:
        print(f"Functional Patterns contract failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
