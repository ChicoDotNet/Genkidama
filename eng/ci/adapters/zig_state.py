#!/usr/bin/env python3
from __future__ import annotations

from debt_contracts import ContractError, ROOT, last_line, require, run


def main() -> int:
    source = ROOT / "src/Systems/Zig/state.zig"
    require(source.is_file(), "Zig State canonical is missing")

    run(["zig", "version"])
    run(["zig", "fmt", "--check", str(source)])
    output = last_line(run(["zig", "run", str(source)], capture=True))
    require(output == "zig-state: passed", f"Zig State output mismatch: {output!r}")

    print("Zig State canonical: PASS", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ContractError as exc:
        print(f"Zig State canonical failed: {exc}", flush=True)
        raise SystemExit(1)
