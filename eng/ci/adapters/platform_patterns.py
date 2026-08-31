#!/usr/bin/env python3
from __future__ import annotations

import os
import sys
import tempfile
from pathlib import Path

import debt_contracts as dc

ASSEMBLY_CONTRACTS: dict[str, str] = {
    "adapter.asm": "legacy=86F\nadapted=30C",
    "bridge.asm": "basic-tv=TV:on\nbasic-radio=Radio:on\nmute-tv=TV:muted\nmute-radio=Radio:muted",
    "builder.asm": "# Service status\n## Availability\n99.95%\n---\n<h1>Service status</h1>\n<h2>Availability</h2><p>99.95%</p>",
    "chain_of_responsibility.asm": "visited=faq>billing;handled=billing;result=refund(250)",
    "composite.asm": "leaf=2\ndocs=8\nroot=10",
    "decorator.asm": "base=alert\naudit=audit(alert)\nencrypted=enc(alert)\nstacked=audit(enc(alert))",
    "example1.asm": "Dark Button\nDark Checkbox\nLight Button\nLight Checkbox",
    "facade.asm": "checkout=auth(alice)>reserve(SKU-42)>charge(499)",
    "factory_method.asm": "PostgreSQL connect\nPostgreSQL query\nMySQL connect\nMySQL query",
    "flyweight.asm": "styles=2;shared=true;text=ABC",
    "mediator.asm": "Assembly Mediator: passed",
    "prototype.asm": "original=orders: metrics\nclone=orders-canary: metrics,tracing",
    "proxy.asm": "backend=1;fetches=1;first=doc(42);second=doc(42)",
    "singleton.asm": "same=true\ncount=1",
}


def normalized(text: str) -> str:
    return text.replace("\r\n", "\n").rstrip("\n")


def validate_assembly() -> None:
    root = dc.ROOT / "src/LowLevel/Assembly"
    historical = {path.name for path in root.glob("*.asm") if not path.name.startswith("example")}
    historical.add("example1.asm")
    dc.require(set(ASSEMBLY_CONTRACTS) == historical, f"Assembly certified inventory changed: expected={sorted(ASSEMBLY_CONTRACTS)} actual={sorted(historical)}")
    with tempfile.TemporaryDirectory(prefix="genkidama-assembly-contract-") as temp:
        work = Path(temp)
        for index, (filename, expected) in enumerate(ASSEMBLY_CONTRACTS.items()):
            source = root / filename
            obj = work / f"contract-{index}.o"
            binary = work / f"contract-{index}"
            dc.run(["nasm", "-f", "elf64", "-Wall", str(source), "-o", str(obj)])
            dc.run(["ld", str(obj), "-o", str(binary)])
            output = normalized(dc.run([str(binary)], capture=True))
            dc.require(output == expected, f"Assembly {filename} output mismatch: expected={expected!r} actual={output!r}")
            print(f"PASS Assembly {filename}", flush=True)


def validate_portable() -> None:
    dc.run([sys.executable, "eng/ci/adapters/platform_source_contracts.py"])
    validate_assembly()

    godot = os.environ.get("GENKIDAMA_GODOT_BIN", "godot")
    output = dc.run([godot, "--headless", "--script", str(dc.ROOT / "src/Niche/GDScript/example1.gd")], capture=True)
    for marker in ["Dark Button", "Dark Checkbox", "Light Button", "Light Checkbox"]:
        dc.require(marker in output.splitlines(), f"GDScript contract missing {marker}")
    mediator_output = dc.run([godot, "--headless", "--script", str(dc.ROOT / "src/Niche/GDScript/mediator.gd")], capture=True)
    dc.require("GDScript Mediator: passed" in mediator_output.splitlines(), "GDScript Mediator canonical output mismatch")

    micropython = os.environ.get("GENKIDAMA_MICROPYTHON_BIN", "/tmp/micropython/ports/unix/build-standard/micropython")
    output = dc.run([micropython, str(dc.ROOT / "src/Other/MicroPython/example1.py")], capture=True)
    for marker in ["Dark Button", "Dark Checkbox", "Light Button", "Light Checkbox"]:
        dc.require(marker in output.splitlines(), f"MicroPython contract missing {marker}")
    mediator_output = dc.run([micropython, str(dc.ROOT / "src/Other/MicroPython/mediator.py")], capture=True)
    dc.require(dc.last_line(mediator_output) == "MicroPython Mediator: passed", "MicroPython Mediator canonical output mismatch")

    rockstar = os.environ.get("GENKIDAMA_ROCKSTAR_BIN")
    dc.require(bool(rockstar), "GENKIDAMA_ROCKSTAR_BIN is required")
    output = dc.run([rockstar, str(dc.ROOT / "src/Other/Rockstar/example1.rock")], capture=True)
    for marker in ["Dark Button", "Dark Checkbox", "Light Button", "Light Checkbox"]:
        dc.require(marker in output.splitlines(), f"Rockstar contract missing {marker}")
    mediator_output = dc.run([rockstar, str(dc.ROOT / "src/Other/Rockstar/mediator.rock")], capture=True)
    dc.require(dc.last_line(mediator_output) == "Rockstar Mediator: passed", "Rockstar Mediator canonical output mismatch")


def main() -> int:
    profile = os.environ.get("GENKIDAMA_PLATFORM_PROFILE", "portable").strip().lower()
    if profile == "portable":
        validate_portable()
    elif profile == "matlab":
        # Local developer path when a MATLAB license is available. GitHub-hosted
        # runners invoke the same .m contract through matlab-actions/run-command.
        dc.run(["matlab", "-batch", "run('eng/ci/adapters/matlab_contract.m')"])
    else:
        raise dc.ContractError(f"unsupported platform profile: {profile}")
    print(f"Platform Patterns contract: PASS profile={profile}", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except dc.ContractError as exc:
        print(f"Platform Patterns contract failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
