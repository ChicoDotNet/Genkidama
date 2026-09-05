#!/usr/bin/env python3
from __future__ import annotations

import tempfile
from pathlib import Path

from debt_contracts import ContractError, ROOT, last_line, require, run
from zig_state import main as validate_zig_state


def main() -> int:
    source = ROOT / "src/Systems/Objective-C/state.m"
    require(source.is_file(), "Objective-C State canonical is missing")

    headers = run(["gcc", "-print-file-name=include"], capture=True).strip()
    libobjc = run(["gcc", "-print-file-name=libobjc.so"], capture=True).strip()
    require(Path(libobjc).is_file(), f"Objective-C runtime library missing: {libobjc}")
    flags = run(["gnustep-config", "--objc-flags"], capture=True).split()
    libs = run(["gnustep-config", "--base-libs"], capture=True).split()

    with tempfile.TemporaryDirectory(prefix="genkidama-objective-c-state-") as temp:
        binary = Path(temp) / "objective-c-state"
        run(
            [
                "clang",
                *flags,
                "-Wall",
                "-Wextra",
                "-Werror",
                f"-I{headers}",
                str(source),
                "-o",
                str(binary),
                f"-L{Path(libobjc).parent}",
                *libs,
            ]
        )
        output = last_line(run([str(binary)], capture=True))
        require(output == "objective-c-state: passed", f"Objective-C State output mismatch: {output!r}")

    print("Objective-C State canonical: PASS", flush=True)
    validate_zig_state()
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ContractError as exc:
        print(f"State canonical validation failed: {exc}", flush=True)
        raise SystemExit(1)
