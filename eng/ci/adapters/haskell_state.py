#!/usr/bin/env python3
from __future__ import annotations

import tempfile
from pathlib import Path

from debt_contracts import ContractError, ROOT, last_line, require, run


def main() -> int:
    source = ROOT / "src/Functional/Haskell/State.hs"
    require(source.is_file(), "Haskell State canonical is missing")

    with tempfile.TemporaryDirectory(prefix="genkidama-haskell-state-") as temp:
        work = Path(temp)
        binary = work / "haskell-state"
        run([
            "ghc",
            "-Wall",
            "-Werror",
            "-O0",
            "-odir",
            str(work),
            "-hidir",
            str(work),
            str(source),
            "-o",
            str(binary),
        ])
        output = last_line(run([str(binary)], capture=True))
        require(output == "haskell-state: passed", f"Haskell State output mismatch: {output!r}")

    print("Haskell State canonical: PASS", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ContractError as exc:
        print(f"Haskell State canonical failed: {exc}", flush=True)
        raise SystemExit(1)
