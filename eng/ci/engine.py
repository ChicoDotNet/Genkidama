#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable

HERE = Path(__file__).resolve().parent
DEFAULT_ROOT = HERE.parents[1]
REGISTRY_PATH = HERE / "registry.json"
PHASES = ("setup", "validation")


class CiError(RuntimeError):
    pass


@dataclass(frozen=True)
class RunOutcome:
    target: str
    status: str
    exit_code: int
    setup_seconds: float
    validation_seconds: float
    total_seconds: float

    def as_dict(self) -> dict[str, Any]:
        return {
            "target": self.target,
            "status": self.status,
            "exit_code": self.exit_code,
            "setup_seconds": round(self.setup_seconds, 3),
            "validation_seconds": round(self.validation_seconds, 3),
            "total_seconds": round(self.total_seconds, 3),
        }


def load_registry(path: Path = REGISTRY_PATH) -> dict[str, Any]:
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise CiError(f"Unable to load CI registry {path}: {exc}") from exc

    if data.get("schema_version") != 1:
        raise CiError("Unsupported CI registry schema_version; expected 1")
    if not isinstance(data.get("targets"), dict):
        raise CiError("CI registry is missing targets")
    if not isinstance(data.get("families"), dict):
        raise CiError("CI registry is missing families")
    return data


def repo_root(explicit: str | None = None) -> Path:
    root = Path(explicit).resolve() if explicit else DEFAULT_ROOT
    if not (root / "Genkidama.slnx").exists():
        raise CiError(f"Repository root is invalid: {root}")
    return root


def _expand_token(token: str, root: Path) -> str:
    return token.replace("{python}", sys.executable).replace("{repo}", str(root))


def _normalize_command(spec: dict[str, Any], root: Path) -> tuple[list[str], Path, dict[str, str]]:
    argv = spec.get("argv")
    if not isinstance(argv, list) or not argv or not all(isinstance(item, str) for item in argv):
        raise CiError(f"Invalid command argv in registry: {spec!r}")

    expanded = [_expand_token(item, root) for item in argv]
    cwd_value = spec.get("cwd", ".")
    if not isinstance(cwd_value, str):
        raise CiError(f"Invalid command cwd in registry: {spec!r}")
    cwd = (root / cwd_value).resolve()

    env = os.environ.copy()
    extra_env = spec.get("env", {})
    if not isinstance(extra_env, dict) or not all(isinstance(k, str) and isinstance(v, str) for k, v in extra_env.items()):
        raise CiError(f"Invalid command env in registry: {spec!r}")
    env.update({key: _expand_token(value, root) for key, value in extra_env.items()})
    return expanded, cwd, env


def _display_command(argv: Iterable[str]) -> str:
    def quote(value: str) -> str:
        if not value or any(ch.isspace() or ch in "'\"" for ch in value):
            return json.dumps(value)
        return value

    return " ".join(quote(value) for value in argv)


def _emit_group_start(label: str) -> None:
    if os.environ.get("GITHUB_ACTIONS") == "true":
        print(f"::group::{label}", flush=True)
    else:
        print(f"==> {label}", flush=True)


def _emit_group_end() -> None:
    if os.environ.get("GITHUB_ACTIONS") == "true":
        print("::endgroup::", flush=True)


def _write_telemetry(payload: dict[str, Any], path: str | None) -> None:
    line = json.dumps(payload, sort_keys=True)
    print(f"CI_TELEMETRY {line}", flush=True)
    destination = path or os.environ.get("GENKIDAMA_CI_TELEMETRY")
    if destination:
        telemetry_path = Path(destination)
        telemetry_path.parent.mkdir(parents=True, exist_ok=True)
        with telemetry_path.open("a", encoding="utf-8") as handle:
            handle.write(line + "\n")


def run_target(
    target_name: str,
    *,
    root: Path,
    registry: dict[str, Any],
    dry_run: bool = False,
    telemetry_file: str | None = None,
) -> RunOutcome:
    target = registry["targets"].get(target_name)
    if not isinstance(target, dict):
        raise CiError(f"Unknown CI target: {target_name}")

    phase_times = {phase: 0.0 for phase in PHASES}
    total_started = time.perf_counter()
    exit_code = 0

    for phase in PHASES:
        commands = target.get(phase, [])
        if not isinstance(commands, list):
            raise CiError(f"Target {target_name!r} has invalid phase {phase!r}")

        phase_started = time.perf_counter()
        for index, spec in enumerate(commands, start=1):
            if not isinstance(spec, dict):
                raise CiError(f"Target {target_name!r} has invalid command in {phase}")
            argv, cwd, env = _normalize_command(spec, root)
            label = spec.get("label") or f"{target_name}:{phase}:{index}"
            _emit_group_start(str(label))
            print(f"$ {_display_command(argv)}", flush=True)
            if dry_run:
                _emit_group_end()
                continue

            if not cwd.exists():
                print(f"Working directory does not exist: {cwd}", file=sys.stderr, flush=True)
                exit_code = 2
                _emit_group_end()
                break

            try:
                completed = subprocess.run(argv, cwd=cwd, env=env, check=False)
                exit_code = completed.returncode
            except FileNotFoundError:
                print(f"Command not found: {argv[0]}", file=sys.stderr, flush=True)
                exit_code = 127

            _emit_group_end()
            if exit_code != 0:
                print(
                    f"Target {target_name} failed in {phase}: {label} (exit {exit_code})",
                    file=sys.stderr,
                    flush=True,
                )
                break

        phase_times[phase] = time.perf_counter() - phase_started
        if exit_code != 0:
            break

    total_seconds = time.perf_counter() - total_started
    status = "dry-run" if dry_run else ("passed" if exit_code == 0 else "failed")
    outcome = RunOutcome(
        target=target_name,
        status=status,
        exit_code=exit_code,
        setup_seconds=phase_times["setup"],
        validation_seconds=phase_times["validation"],
        total_seconds=total_seconds,
    )
    _write_telemetry(outcome.as_dict(), telemetry_file)
    return outcome


def _path_matches(path: str, entries: Iterable[str]) -> bool:
    for entry in entries:
        if entry.endswith("/"):
            if path.startswith(entry):
                return True
        elif path == entry:
            return True
    return False


def classify_paths(paths: Iterable[str], registry: dict[str, Any]) -> dict[str, Any]:
    config = registry.get("change_detection", {})
    family_paths = config.get("family_paths", {})
    family_names = sorted(registry["families"].keys())

    result: dict[str, Any] = {
        "product": False,
        "quality": False,
        "polyglot": [],
        "learn_languages": [],
        "full": False,
        "unknown_paths": [],
    }
    polyglot: set[str] = set()
    learn_languages: set[str] = set()

    normalized_paths = [path.strip().replace("\\", "/") for path in paths if path.strip()]
    if not normalized_paths:
        return result

    for path in normalized_paths:
        matched = False

        if _path_matches(path, config.get("full_paths", [])):
            result["full"] = True
            matched = True

        if _path_matches(path, config.get("product_paths", [])):
            result["product"] = True
            matched = True

        if _path_matches(path, config.get("quality_paths", [])):
            result["quality"] = True
            matched = True

        learn_root = config.get("learn_root", "learn/es/")
        if isinstance(learn_root, str) and path.startswith(learn_root):
            remainder = path[len(learn_root):]
            language = remainder.split("/", 1)[0]
            if language:
                learn_languages.add(language)
            result["quality"] = True
            matched = True

        for family, entries in family_paths.items():
            if family not in registry["families"]:
                raise CiError(f"change_detection references unknown family {family!r}")
            if _path_matches(path, entries):
                polyglot.add(family)
                matched = True

        if not matched:
            result["unknown_paths"].append(path)

    if result["unknown_paths"]:
        result["full"] = True

    if result["full"]:
        result["product"] = True
        result["quality"] = True
        polyglot.update(family_names)

    result["polyglot"] = sorted(polyglot)
    result["learn_languages"] = sorted(learn_languages)
    return result


def changed_paths_from_git(root: Path, base: str, head: str) -> list[str]:
    completed = subprocess.run(
        ["git", "diff", "--name-only", "--diff-filter=ACMR", base, head],
        cwd=root,
        text=True,
        capture_output=True,
        check=False,
    )
    if completed.returncode != 0:
        message = completed.stderr.strip() or completed.stdout.strip() or "git diff failed"
        raise CiError(message)
    return [line for line in completed.stdout.splitlines() if line.strip()]


def _add_common_run_options(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("--repo-root", help="Repository root. Defaults to the repository containing eng/ci.")
    parser.add_argument("--dry-run", action="store_true", help="Print commands without executing them.")
    parser.add_argument("--telemetry-file", help="Append telemetry JSON to this file.")


def _run_named_target(target: str, argv: list[str] | None) -> int:
    parser = argparse.ArgumentParser()
    _add_common_run_options(parser)
    args = parser.parse_args(argv)
    root = repo_root(args.repo_root)
    outcome = run_target(
        target,
        root=root,
        registry=load_registry(),
        dry_run=args.dry_run,
        telemetry_file=args.telemetry_file,
    )
    return outcome.exit_code


def main_product(argv: list[str] | None = None) -> int:
    return _run_named_target("product", argv)


def main_quality(argv: list[str] | None = None) -> int:
    return _run_named_target("quality", argv)


def main_family(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Run one polyglot runtime family locally.")
    parser.add_argument("family")
    surface = parser.add_mutually_exclusive_group()
    surface.add_argument("--learn", action="store_true", help="Run the Learn surface for this family.")
    surface.add_argument("--patterns", action="store_true", help="Run the Patterns surface for this family.")
    surface.add_argument("--all", action="store_true", help="Run all registered surfaces for this family.")
    _add_common_run_options(parser)
    args = parser.parse_args(argv)

    registry = load_registry()
    family = registry["families"].get(args.family)
    if not isinstance(family, dict):
        available = ", ".join(sorted(registry["families"]))
        raise CiError(f"Unknown family {args.family!r}. Available: {available}")

    if args.learn:
        requested = ["learn"]
    elif args.patterns:
        requested = ["patterns"]
    else:
        requested = sorted(family.keys())

    root = repo_root(args.repo_root)
    ran = False
    for surface_name in requested:
        target = family.get(surface_name)
        if not target:
            if len(requested) == 1:
                raise CiError(f"Family {args.family!r} has no {surface_name!r} surface yet")
            continue
        ran = True
        outcome = run_target(
            target,
            root=root,
            registry=registry,
            dry_run=args.dry_run,
            telemetry_file=args.telemetry_file,
        )
        if outcome.exit_code != 0:
            return outcome.exit_code

    if not ran:
        raise CiError(f"Family {args.family!r} has no runnable surfaces")
    return 0


def main_detect_changes(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Map a git diff to Genkidama CI gates and runtime families.")
    parser.add_argument("base", nargs="?")
    parser.add_argument("head", nargs="?")
    parser.add_argument("--repo-root", help="Repository root. Defaults to the repository containing eng/ci.")
    parser.add_argument("--path", dest="paths", action="append", default=[], help="Classify an explicit changed path; repeatable.")
    parser.add_argument("--full", action="store_true", help="Force every currently registered gate/family.")
    args = parser.parse_args(argv)

    root = repo_root(args.repo_root)
    registry = load_registry()

    if args.full:
        result = classify_paths(["eng/ci/forced-full"], registry)
    elif args.paths:
        result = classify_paths(args.paths, registry)
    else:
        if not args.base or not args.head:
            parser.error("BASE and HEAD are required unless --path or --full is used")
        result = classify_paths(changed_paths_from_git(root, args.base, args.head), registry)

    print(json.dumps(result, sort_keys=True))
    return 0


def _guard(entrypoint) -> None:
    try:
        raise SystemExit(entrypoint())
    except CiError as exc:
        print(f"CI configuration error: {exc}", file=sys.stderr)
        raise SystemExit(2)
