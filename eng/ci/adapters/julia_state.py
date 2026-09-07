#!/usr/bin/env python3
from __future__ import annotations

from debt_contracts import ContractError, ROOT, last_line, require, run


def main() -> int:
    source = ROOT / "src/DataScience/Julia/state.jl"
    require(source.is_file(), "Julia State canonical is missing")

    run(["julia", "--version"])
    output = last_line(
        run(
            ["julia", "--startup-file=no", "--check-bounds=yes", str(source)],
            capture=True,
        )
    )
    require(output == "julia-state: passed", f"Julia State output mismatch: {output!r}")

    print("Julia State canonical: PASS", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ContractError as exc:
        print(f"Julia State canonical failed: {exc}", flush=True)
        raise SystemExit(1)
