#!/usr/bin/env python3
from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

import debt_contracts as dc


def run_cmd(command: str, *, cwd: Path, env: dict[str, str] | None = None) -> None:
    merged = os.environ.copy()
    if env:
        merged.update(env)
    print(f"> cmd.exe /d /s /c {command}", flush=True)
    completed = subprocess.run(
        ["cmd.exe", "/d", "/s", "/c", command],
        cwd=cwd,
        env=merged,
        check=False,
    )
    if completed.returncode != 0:
        raise dc.ContractError(f"Windows command failed with exit {completed.returncode}: {command}")


def validate_windows() -> None:
    ruby = dc.ROOT / "learn/es/ruby/app"
    run_cmd("bundle install", cwd=ruby)
    rails_env = {"RAILS_ENV": "test"}
    run_cmd("bundle exec rails db:prepare", cwd=ruby, env=rails_env)
    run_cmd("bundle exec rails test", cwd=ruby, env=rails_env)
    run_cmd("bundle exec rails runner script/smoke.rb", cwd=ruby, env=rails_env)
    dc.powershell_contract()


def main() -> int:
    profile = os.environ.get("GENKIDAMA_SCRIPTING_PROFILE", "linux").strip().lower()
    if profile == "linux":
        dc.learn_scripting()
    elif profile == "windows":
        validate_windows()
    else:
        raise dc.ContractError(f"unsupported scripting profile: {profile}")
    print(f"Scripting Learn contract: PASS profile={profile}", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except dc.ContractError as exc:
        print(f"Scripting Learn contract failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
