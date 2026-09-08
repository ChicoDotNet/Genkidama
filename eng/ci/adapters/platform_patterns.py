#!/usr/bin/env python3
from __future__ import annotations

import hashlib
import os
import shutil
import sqlite3
import sys
import tarfile
import tempfile
import urllib.request
from pathlib import Path

import debt_contracts as dc

NASM_VERSION = "3.02"
NASM_MD5 = "31ba022fff8de3065b5bc5ad5559188c"
NASM_URL = f"https://www.nasm.us/pub/nasm/releasebuilds/{NASM_VERSION}/nasm-{NASM_VERSION}.tar.xz"

ASSEMBLY_CONTRACTS: dict[str, str] = {
    "adapter.asm": "legacy=86F\nadapted=30C",
    "bridge.asm": "basic-tv=TV:on\nbasic-radio=Radio:on\nmute-tv=TV:muted\nmute-radio=Radio:muted",
    "builder.asm": "# Service status\n## Availability\n99.95%\n---\n<h1>Service status</h1>\n<h2>Availability</h2><p>99.95%</p>",
    "chain_of_responsibility.asm": "visited=faq>billing;handled=billing;result=refund(250)",
    "command.asm": "balance=130;commands=2",
    "composite.asm": "leaf=2\ndocs=8\nroot=10",
    "decorator.asm": "base=alert\naudit=audit(alert)\nencrypted=enc(alert)\nstacked=audit(enc(alert))",
    "example1.asm": "Dark Button\nDark Checkbox\nLight Button\nLight Checkbox",
    "facade.asm": "checkout=auth(alice)>reserve(SKU-42)>charge(499)",
    "factory_method.asm": "PostgreSQL connect\nPostgreSQL query\nMySQL connect\nMySQL query",
    "flyweight.asm": "styles=2;shared=true;text=ABC",
    "observer.asm": "audit=2;dashboard=1;duplicate=rejected;second-unsubscribe=rejected",
    "interpreter.asm": "interpreter=9",
    "iterator.asm": "iterator=10,20,30",
    "mediator.asm": "Assembly Mediator: passed",
    "memento.asm": "Assembly Memento: passed",
    "prototype.asm": "original=orders: metrics\nclone=orders-canary: metrics,tracing",
    "proxy.asm": "backend=1;fetches=1;first=doc(42);second=doc(42)",
    "singleton.asm": "same=true\ncount=1",
    "state.asm": "locked\nlocked\nunlocked\nunlocked\nlocked\ninvalid\nassembly-state: passed",
    "strategy.asm": "strategy=regular:100;vip:80;campaign:75;below:80",
}


def normalized(text: str) -> str:
    return text.replace("\r\n", "\n").rstrip("\n")


def ensure_stable_nasm() -> Path:
    configured = os.environ.get("GENKIDAMA_NASM_BIN")
    if configured:
        nasm = Path(configured)
        dc.require(nasm.is_file(), f"configured NASM binary does not exist: {nasm}")
    else:
        runner_temp = Path(os.environ.get("RUNNER_TEMP", "/tmp"))
        prefix = runner_temp / f"nasm-{NASM_VERSION}-install"
        nasm = prefix / "bin" / "nasm"
        if not nasm.is_file():
            archive = runner_temp / f"nasm-{NASM_VERSION}.tar.xz"
            source = runner_temp / f"nasm-{NASM_VERSION}"
            archive.unlink(missing_ok=True)
            if source.exists():
                shutil.rmtree(source)
            print(f"Downloading NASM {NASM_VERSION} from nasm.us", flush=True)
            urllib.request.urlretrieve(NASM_URL, archive)
            digest = hashlib.md5(archive.read_bytes()).hexdigest()
            dc.require(digest == NASM_MD5, f"NASM {NASM_VERSION} MD5 mismatch: {digest}")
            with tarfile.open(archive, "r:xz") as tar:
                tar.extractall(runner_temp, filter="data")
            dc.run(["./configure", f"--prefix={prefix}"], cwd=source)
            dc.run(["make", "-j2"], cwd=source)
            dc.run(["make", "install"], cwd=source)

    version = dc.run([str(nasm), "-v"], capture=True).strip()
    dc.require(version.startswith(f"NASM version {NASM_VERSION}"), f"expected NASM {NASM_VERSION}, got {version}")
    print(f"NASM toolchain: {version}", flush=True)
    return nasm


def validate_assembly() -> None:
    root = dc.ROOT / "src/LowLevel/Assembly"
    historical = {path.name for path in root.glob("*.asm") if not path.name.startswith("example")}
    historical.add("example1.asm")
    dc.require(set(ASSEMBLY_CONTRACTS) == historical, f"Assembly certified inventory changed: expected={sorted(ASSEMBLY_CONTRACTS)} actual={sorted(historical)}")
    nasm = ensure_stable_nasm()
    with tempfile.TemporaryDirectory(prefix="genkidama-assembly-contract-") as temp:
        work = Path(temp)
        for index, (filename, expected) in enumerate(ASSEMBLY_CONTRACTS.items()):
            source = root / filename
            obj = work / f"contract-{index}.o"
            binary = work / f"contract-{index}"
            dc.run([str(nasm), "-f", "elf64", "-Wall", str(source), "-o", str(obj)])
            dc.run(["ld", str(obj), "-o", str(binary)])
            output = normalized(dc.run([str(binary)], capture=True))
            dc.require(output == expected, f"Assembly {filename} output mismatch: expected={expected!r} actual={output!r}")
            print(f"PASS Assembly {filename}", flush=True)


def validate_vba_state() -> None:
    source = dc.ROOT / "src/Shell/VBA/state.bas"
    dc.require(source.is_file(), "VBA State canonical missing")
    text = source.read_text(encoding="utf-8")
    lower = text.lower()
    required = [
        "option explicit",
        "private enum gatestate",
        "gatelocked = 0",
        "gateunlocked = 1",
        "private function transition",
        "select case currentstate",
        'if action = "coin" then',
        'if action = "push" then',
        "err.raise vbobjecterror + 513",
        "public sub verifystatepattern()",
        'requirestate state = gatelocked, "push while locked must preserve state"',
        'requirestate state = gateunlocked, "duplicate coin must preserve unlocked state"',
        'debug.print "vba-state: passed"',
    ]
    for marker in required:
        dc.require(marker in lower, f"VBA State source contract missing {marker!r}")
    dc.require(lower.count("state = transition(state,") == 4, "VBA State contract must exercise four transition decisions")
    print("PASS VBA state.bas source contract", flush=True)


def validate_sql_observer() -> None:
    source = (dc.ROOT / "src/Data/SQL/observer.sql").read_text(encoding="utf-8")
    connection = sqlite3.connect(":memory:")
    try:
        row = connection.execute(source).fetchone()
    finally:
        connection.close()
    dc.require(row == ("SQL Observer: passed",), f"SQL Observer contract failed: {row!r}")
    print("PASS SQL observer.sql", flush=True)


def validate_sql_memento() -> None:
    source = (dc.ROOT / "src/Data/SQL/memento.sql").read_text(encoding="utf-8")
    connection = sqlite3.connect(":memory:")
    try:
        row = connection.execute(source).fetchone()
    finally:
        connection.close()
    dc.require(row == ("SQL Memento: passed",), f"SQL Memento contract failed: {row!r}")
    print("PASS SQL memento.sql", flush=True)


def validate_sql_state() -> None:
    source = dc.ROOT / "src/Data/SQL/state.sql"
    dc.require(source.is_file(), "SQL State canonical missing")
    connection = sqlite3.connect(":memory:")
    try:
        connection.executescript(source.read_text(encoding="utf-8"))
        trace = connection.execute("SELECT step, state FROM state_trace ORDER BY step").fetchall()
        expected = [
            (0, "locked"),
            (1, "locked"),
            (2, "unlocked"),
            (3, "unlocked"),
            (4, "locked"),
        ]
        dc.require(trace == expected, f"SQL State trace mismatch: expected={expected!r} actual={trace!r}")
        invalid = connection.execute("SELECT observed_state FROM invalid_state_probe").fetchone()
        dc.require(invalid == ("invalid",), f"SQL State invalid-state contract mismatch: {invalid!r}")
    finally:
        connection.close()
    print("PASS SQL state.sql via sqlite3", flush=True)


def validate_gdscript_state(godot: str) -> None:
    source = dc.ROOT / "src/Niche/GDScript/state.gd"
    dc.require(source.is_file(), "GDScript State canonical missing")
    output = dc.run([godot, "--headless", "--script", str(source)], capture=True)
    dc.require("gdscript-state: passed" in output.splitlines(), "GDScript State canonical output mismatch")
    print("PASS GDScript state.gd", flush=True)


def validate_gdscript_strategy(godot: str) -> None:
    source = dc.ROOT / "src/Niche/GDScript/strategy.gd"
    dc.require(source.is_file(), "GDScript Strategy canonical missing")
    output = dc.run([godot, "--headless", "--script", str(source)], capture=True)
    dc.require("GDScript Strategy: passed" in output.splitlines(), "GDScript Strategy behavioral contract failed")
    print("PASS GDScript strategy.gd", flush=True)


def validate_micropython_state(micropython: str) -> None:
    source = dc.ROOT / "src/Other/MicroPython/state.py"
    dc.require(source.is_file(), "MicroPython State canonical missing")
    output = dc.run([micropython, str(source)], capture=True)
    dc.require("MicroPython State: passed" in output.splitlines(), "MicroPython State canonical output mismatch")
    print("PASS MicroPython state.py", flush=True)


def validate_micropython_strategy(micropython: str) -> None:
    source = dc.ROOT / "src/Other/MicroPython/strategy.py"
    dc.require(source.is_file(), "MicroPython Strategy canonical missing")
    output = dc.run([micropython, str(source)], capture=True)
    dc.require("MicroPython Strategy: passed" in output.splitlines(), "MicroPython Strategy canonical output mismatch")
    print("PASS MicroPython strategy.py", flush=True)


def validate_rockstar_state(rockstar: str) -> None:
    source = dc.ROOT / "src/Other/Rockstar/state.rock"
    dc.require(source.is_file(), "Rockstar State canonical missing")
    output = normalized(dc.run([rockstar, str(source)], capture=True))
    expected = "locked\nlocked\nunlocked\nunlocked\nlocked\ninvalid\nrockstar-state: passed"
    dc.require(output == expected, f"Rockstar State canonical output mismatch: expected={expected!r} actual={output!r}")
    print("PASS Rockstar state.rock", flush=True)


def validate_portable() -> None:
    dc.run([sys.executable, "eng/ci/adapters/platform_source_contracts.py"])
    validate_vba_state()
    validate_assembly()
    validate_sql_observer()
    validate_sql_memento()
    validate_sql_state()

    godot = os.environ.get("GENKIDAMA_GODOT_BIN", "godot")
    output = dc.run([godot, "--headless", "--script", str(dc.ROOT / "src/Niche/GDScript/example1.gd")], capture=True)
    for marker in ["Dark Button", "Dark Checkbox", "Light Button", "Light Checkbox"]:
        dc.require(marker in output.splitlines(), f"GDScript contract missing {marker}")
    mediator_output = dc.run([godot, "--headless", "--script", str(dc.ROOT / "src/Niche/GDScript/mediator.gd")], capture=True)
    dc.require("GDScript Mediator: passed" in mediator_output.splitlines(), "GDScript Mediator canonical output mismatch")
    memento_output = dc.run([godot, "--headless", "--script", str(dc.ROOT / "src/Niche/GDScript/memento.gd")], capture=True)
    dc.require("GDScript Memento: passed" in memento_output.splitlines(), "GDScript Memento contract failed")
    observer_output = dc.run([godot, "--headless", "--script", str(dc.ROOT / "src/Niche/GDScript/observer.gd")], capture=True)
    observer_marker = "observer=audit:draft,published;dashboard:draft;duplicate=rejected;second-unsubscribe=rejected"
    dc.require(observer_marker in observer_output.splitlines(), "GDScript Observer behavioral contract failed")
    validate_gdscript_state(godot)
    validate_gdscript_strategy(godot)

    micropython = os.environ.get("GENKIDAMA_MICROPYTHON_BIN", "/tmp/micropython/ports/unix/build-standard/micropython")
    output = dc.run([micropython, str(dc.ROOT / "src/Other/MicroPython/example1.py")], capture=True)
    for marker in ["Dark Button", "Dark Checkbox", "Light Button", "Light Checkbox"]:
        dc.require(marker in output.splitlines(), f"MicroPython contract missing {marker}")
    observer_output = dc.run([micropython, str(dc.ROOT / "src/Other/MicroPython/observer.py")], capture=True)
    observer_marker = "observer=audit:draft,published;dashboard:draft;duplicate=rejected;second-unsubscribe=rejected"
    dc.require(observer_marker in observer_output.splitlines(), "MicroPython Observer behavioral contract failed")
    mediator_output = dc.run([micropython, str(dc.ROOT / "src/Other/MicroPython/mediator.py")], capture=True)
    dc.require(dc.last_line(mediator_output) == "MicroPython Mediator: passed", "MicroPython Mediator canonical output mismatch")
    memento_output = dc.run([micropython, str(dc.ROOT / "src/Other/MicroPython/memento.py")], capture=True)
    dc.require(dc.last_line(memento_output) == "MicroPython Memento: passed", "MicroPython Memento contract failed")
    validate_micropython_state(micropython)
    validate_micropython_strategy(micropython)

    rockstar = os.environ.get("GENKIDAMA_ROCKSTAR_BIN")
    dc.require(bool(rockstar), "GENKIDAMA_ROCKSTAR_BIN is required")
    output = dc.run([rockstar, str(dc.ROOT / "src/Other/Rockstar/example1.rock")], capture=True)
    for marker in ["Dark Button", "Dark Checkbox", "Light Button", "Light Checkbox"]:
        dc.require(marker in output.splitlines(), f"Rockstar contract missing {marker}")
    observer_output = dc.run([rockstar, str(dc.ROOT / "src/Other/Rockstar/observer.rock")], capture=True)
    observer_marker = "observer=audit:draft,published;dashboard:draft;duplicate=rejected;second-unsubscribe=rejected"
    dc.require(observer_marker in observer_output.splitlines(), "Rockstar Observer behavioral contract failed")
    mediator_output = dc.run([rockstar, str(dc.ROOT / "src/Other/Rockstar/mediator.rock")], capture=True)
    dc.require(dc.last_line(mediator_output) == "Rockstar Mediator: passed", "Rockstar Mediator canonical output mismatch")
    memento_output = dc.run([rockstar, str(dc.ROOT / "src/Other/Rockstar/memento.rock")], capture=True)
    dc.require(dc.last_line(memento_output) == "Rockstar Memento: passed", "Rockstar Memento contract failed")
    validate_rockstar_state(rockstar)


def main() -> int:
    profile = os.environ.get("GENKIDAMA_PLATFORM_PROFILE", "portable").strip().lower()
    if profile == "portable":
        validate_portable()
    elif profile == "matlab":
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