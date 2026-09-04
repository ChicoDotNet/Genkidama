#!/usr/bin/env python3
from __future__ import annotations

import hashlib
import os
import shutil
import sys
import tarfile
import urllib.request
from pathlib import Path

import debt_contracts as dc


PERL_VERSION = "5.44.0"
PERL_SHA256 = "3b855066b92491cb40e86affb1ca57d1a388aa43e51b91c7806a32c2f65f96c3"
PERL_URL = f"https://www.cpan.org/src/5.0/perl-{PERL_VERSION}.tar.gz"


def ensure_stable_perl() -> Path:
    configured = os.environ.get("GENKIDAMA_PERL_BIN")
    if configured:
        perl = Path(configured)
        dc.require(perl.is_file(), f"configured Perl binary does not exist: {perl}")
    else:
        runner_temp = Path(os.environ.get("RUNNER_TEMP", "/tmp"))
        prefix = runner_temp / f"perl-{PERL_VERSION}-install"
        perl = prefix / "bin" / "perl"
        if not perl.is_file():
            archive = runner_temp / f"perl-{PERL_VERSION}.tar.gz"
            source = runner_temp / f"perl-{PERL_VERSION}"
            if archive.exists():
                archive.unlink()
            if source.exists():
                shutil.rmtree(source)
            print(f"Downloading Perl {PERL_VERSION} from CPAN", flush=True)
            urllib.request.urlretrieve(PERL_URL, archive)
            digest = hashlib.sha256(archive.read_bytes()).hexdigest()
            dc.require(digest == PERL_SHA256, f"Perl {PERL_VERSION} SHA-256 mismatch: {digest}")
            with tarfile.open(archive, "r:gz") as tar:
                tar.extractall(runner_temp, filter="data")
            dc.run(["./Configure", "-des", f"-Dprefix={prefix}"], cwd=source)
            dc.run(["make", "-j2"], cwd=source)
            dc.run(["make", "install"], cwd=source)

    version = dc.run([str(perl), "-e", "print $^V"], capture=True).strip()
    dc.require(version == f"v{PERL_VERSION}", f"expected Perl v{PERL_VERSION}, got {version}")
    print(f"Perl toolchain: {version}", flush=True)
    return perl


def main() -> int:
    profile = os.environ.get("GENKIDAMA_SCRIPTING_PROFILE", "linux").strip().lower()
    if profile == "windows":
        print("Scripting Patterns contract: no Windows-only pattern surface")
        return 0
    if profile != "linux":
        raise dc.ContractError(f"unsupported scripting profile: {profile}")

    py = dc.ROOT / "src/Scripting/PythonPY/pattern_sweep.py"
    dc.run([sys.executable, "-m", "py_compile", str(py)])
    dc.run([sys.executable, "-B", str(py)])
    python_observer = dc.ROOT / "src/Scripting/PythonPY/observer.py"
    dc.run([sys.executable, "-m", "py_compile", str(python_observer)])
    dc.require(
        dc.last_line(dc.run([sys.executable, "-B", str(python_observer)], capture=True)) == "Python Observer: passed",
        "Python Observer canonical output mismatch",
    )

    perl = ensure_stable_perl()
    perl_observer = dc.ROOT / "src/Scripting/Perl/observer.pl"
    dc.run([str(perl), "-c", str(perl_observer)])
    dc.require(
        "OBSERVER_PERL_OK" in dc.run([str(perl), str(perl_observer)], capture=True).splitlines(),
        "Perl Observer behavioral contract failed",
    )

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
