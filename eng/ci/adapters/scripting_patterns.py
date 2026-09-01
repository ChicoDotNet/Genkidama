#!/usr/bin/env python3
from __future__ import annotations

import os
import sys

import debt_contracts as dc


def main() -> int:
    profile = os.environ.get("GENKIDAMA_SCRIPTING_PROFILE", "linux").strip().lower()
    if profile == "windows":
        print("Scripting Patterns contract: no Windows-only pattern surface")
        return 0
    if profile != "linux":
        raise dc.ContractError(f"unsupported scripting profile: {profile}")

    python_memento = dc.ROOT / "src/Scripting/PythonPY/memento.py"
    dc.run([sys.executable, "-m", "py_compile", str(python_memento)])
    dc.require(
        dc.last_line(dc.run([sys.executable, "-B", str(python_memento)], capture=True))
        == "Python Memento: passed",
        "Python Memento canonical output mismatch",
    )

    py = dc.ROOT / "src/Scripting/PythonPY/pattern_sweep.py"
    dc.run([sys.executable, "-m", "py_compile", str(py)])
    dc.run([sys.executable, "-B", str(py)])

    ruby_files = dc.exact_glob(dc.ROOT / "src/Scripting/Ruby/patterns", "*.rb", "Ruby")
    for source in ruby_files:
        dc.run(["ruby", "-c", str(source)])
        dc.run(["ruby", str(source)])
    ruby_aggregate = dc.ROOT / "src/Scripting/Ruby/pattern_sweep.rb"
    dc.run(["ruby", "-c", str(ruby_aggregate)])
    dc.require(dc.last_line(dc.run(["ruby", str(ruby_aggregate)], capture=True)) == "ruby-pattern-sweep: 39/39 passed", "Ruby aggregate output mismatch")

    php_files = dc.exact_glob(dc.ROOT / "src/Scripting/PHP/patterns", "*.php", "PHP")
    for source in php_files:
        dc.run(["php", "-l", str(source)])
        dc.run(["php", str(source)])
    php_aggregate = dc.ROOT / "src/Scripting/PHP/pattern_sweep.php"
    dc.run(["php", "-l", str(php_aggregate)])
    dc.require(dc.last_line(dc.run(["php", str(php_aggregate)], capture=True)) == "php-pattern-sweep: 39/39 passed", "PHP aggregate output mismatch")

    bash_bin = os.environ.get("GENKIDAMA_BASH_BIN", "bash")
    bash_files = dc.exact_glob(dc.ROOT / "src/Scripting/Bash/patterns", "*.sh", "Bash")
    dc.run([bash_bin, "-n", *map(str, bash_files), str(dc.ROOT / "src/Scripting/Bash/pattern_sweep.sh")])
    for source in bash_files:
        dc.run([bash_bin, str(source)])
    output = dc.run([bash_bin, str(dc.ROOT / "src/Scripting/Bash/pattern_sweep.sh")], capture=True)
    dc.require("bash-pattern-sweep: 39/39 passed" in output.splitlines(), "Bash aggregate output mismatch")

    lua = os.environ.get("GENKIDAMA_LUA_BIN", "lua")
    luac = os.environ.get("GENKIDAMA_LUAC_BIN", "luac")
    lua_files = dc.exact_glob(dc.ROOT / "src/Scripting/Lua/patterns", "*.lua", "Lua")
    for source in lua_files:
        dc.run([luac, "-p", str(source)])
        dc.run([lua, str(source)])
    dc.run([lua, str(dc.ROOT / "src/Scripting/Lua/pattern_sweep.lua")])

    dc.run([sys.executable, "eng/ci/adapters/prototype.py", "scripting"])
    print("Scripting Patterns contract: PASS without duplicate PowerShell sweep", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except dc.ContractError as exc:
        print(f"Scripting Patterns contract failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
