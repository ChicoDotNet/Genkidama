#!/usr/bin/env python3
from __future__ import annotations

import difflib
import subprocess
from pathlib import Path

BASE = "505f331b1d10644474beb55a8d8aeb1138fb791a"
OBSERVER = "48cc70c7ecf91d6d8e8f4f350daf12eea46f0a43"

# dart_contracts.py has a true same-block overlap (Memento + Observer) and is
# composed explicitly below. The remaining unresolved files use guarded replay.
PATHS = [
    "eng/ci/adapters/native_patterns.py",
    "eng/ci/adapters/platform_patterns.py",
    "eng/ci/adapters/platform_source_contracts.py",
    "src/DataScience/Julia/pattern_sweep.jl",
    "src/Functional/Haskell/PatternSweep.hs",
    "src/Niche/Crystal/pattern_sweep.cr",
    "src/Systems/Zig/pattern_sweep.zig",
    "src/Web/Dart/pattern_sweep.dart",
]


def git_show(ref: str, path: str) -> list[str]:
    completed = subprocess.run(
        ["git", "show", f"{ref}:{path}"],
        check=True,
        text=True,
        stdout=subprocess.PIPE,
    )
    return completed.stdout.splitlines(keepends=True)


def occurrences(lines: list[str], needle: list[str]) -> list[int]:
    if not needle:
        return []
    width = len(needle)
    return [i for i in range(len(lines) - width + 1) if lines[i : i + width] == needle]


def compact(block: list[str], limit: int = 3) -> str:
    rendered = "".join(block[:limit]).strip().replace("\n", " | ")
    return rendered[:240]


def choose_anchor(lines: list[str], context: list[str], *, prefer_last: bool) -> tuple[int, int] | None:
    if not context:
        return None
    max_width = min(6, len(context))
    for width in range(max_width, 0, -1):
        anchor = context[-width:] if prefer_last else context[:width]
        hits = occurrences(lines, anchor)
        if len(hits) == 1:
            start = hits[0]
            return start, start + width
    return None


def replace_once(path: str, old: str, new: str) -> None:
    target = Path(path)
    text = target.read_text(encoding="utf-8")
    count = text.count(old)
    if count != 1:
        raise SystemExit(f"{path}: expected exactly one explicit composition anchor, found {count}")
    target.write_text(text.replace(old, new, 1), encoding="utf-8")


def compose_dart_contracts() -> None:
    path = "eng/ci/adapters/dart_contracts.py"
    old = '''def patterns() -> None:\n    sweep = ROOT / "src/Web/Dart/pattern_sweep.dart"\n    mediator = ROOT / "src/Web/Dart/mediator.dart"\n    memento = ROOT / "src/Web/Dart/memento.dart"\n    sources = [str(sweep), str(mediator), str(memento)]\n    run(["dart", "format", "--output=none", "--set-exit-if-changed", *sources])\n    run(["dart", "analyze", "--fatal-infos", "--fatal-warnings", *sources])\n    require(\n        last_line(run(["dart", "run", str(mediator)], capture=True)) == "Dart Mediator: passed",\n        "Dart Mediator canonical output mismatch",\n    )\n    require(\n        last_line(run(["dart", "run", str(memento)], capture=True)) == "Dart Memento: passed",\n        "Dart Memento canonical output mismatch",\n    )\n    require(\n        last_line(run(["dart", "run", str(sweep)], capture=True)) == "Dart pattern sweep: 39/39 examples passed",\n        "Dart aggregate output mismatch",\n    )\n'''
    new = '''def patterns() -> None:\n    sweep = ROOT / "src/Web/Dart/pattern_sweep.dart"\n    mediator = ROOT / "src/Web/Dart/mediator.dart"\n    memento = ROOT / "src/Web/Dart/memento.dart"\n    observer = ROOT / "src/Web/Dart/observer.dart"\n    observer_verify = ROOT / "src/Web/Dart/observer_verify.dart"\n    sources = [str(sweep), str(mediator), str(memento), str(observer), str(observer_verify)]\n    run(["dart", "format", "--output=none", "--set-exit-if-changed", *sources])\n    run(["dart", "analyze", "--fatal-infos", "--fatal-warnings", *sources])\n    require(\n        last_line(run(["dart", "run", str(mediator)], capture=True)) == "Dart Mediator: passed",\n        "Dart Mediator canonical output mismatch",\n    )\n    require(\n        last_line(run(["dart", "run", str(memento)], capture=True)) == "Dart Memento: passed",\n        "Dart Memento canonical output mismatch",\n    )\n    require(\n        last_line(run(["dart", "run", str(sweep)], capture=True)) == "Dart pattern sweep: 39/39 examples passed",\n        "Dart aggregate output mismatch",\n    )\n    require(\n        last_line(run(["dart", "run", str(observer_verify)], capture=True)) == "Dart Observer: passed",\n        "Dart Observer output mismatch",\n    )\n'''
    replace_once(path, old, new)
    print(f"{path}: explicit Memento + Observer composition PASS", flush=True)


def replay_edit(
    path: str,
    current: list[str],
    old: list[str],
    new: list[str],
    before: list[str],
    after: list[str],
    tag: str,
) -> list[str]:
    new_hits = occurrences(current, new) if new else []
    old_hits = occurrences(current, old) if old else []

    if old:
        if len(old_hits) == 1:
            start = old_hits[0]
            return current[:start] + new + current[start + len(old) :]
        if len(old_hits) == 0 and new and len(new_hits) == 1:
            print(f"{path}: {tag} already present", flush=True)
            return current
        raise SystemExit(
            f"{path}: {tag} cannot replay replace/delete uniquely; "
            f"old_hits={len(old_hits)} new_hits={len(new_hits)} old={compact(old)!r} new={compact(new)!r}"
        )

    if not new:
        return current
    if len(new_hits) == 1:
        print(f"{path}: {tag} insertion already present", flush=True)
        return current
    if len(new_hits) > 1:
        raise SystemExit(f"{path}: {tag} desired insertion already occurs {len(new_hits)} times")

    left = choose_anchor(current, before, prefer_last=True)
    right = choose_anchor(current, after, prefer_last=False)
    if left is None and right is None:
        raise SystemExit(f"{path}: {tag} has no unique insertion anchor; new={compact(new)!r}")

    insert_at = left[1] if left is not None else right[0]  # type: ignore[index]
    return current[:insert_at] + new + current[insert_at:]


def replay_path(path: str) -> None:
    base = git_show(BASE, path)
    observer = git_show(OBSERVER, path)
    target = Path(path)
    current = target.read_text(encoding="utf-8").splitlines(keepends=True)

    matcher = difflib.SequenceMatcher(a=base, b=observer, autojunk=False)
    edits = [op for op in matcher.get_opcodes() if op[0] != "equal"]
    print(f"{path}: replaying {len(edits)} Observer edit blocks", flush=True)

    for ordinal, (tag, i1, i2, j1, j2) in enumerate(edits, start=1):
        old = base[i1:i2]
        new = observer[j1:j2]
        before = base[max(0, i1 - 8) : i1]
        after = base[i2 : min(len(base), i2 + 8)]
        current = replay_edit(path, current, old, new, before, after, f"edit {ordinal}/{len(edits)} {tag}")

    target.write_text("".join(current), encoding="utf-8")

    final = target.read_text(encoding="utf-8").splitlines(keepends=True)
    matcher = difflib.SequenceMatcher(a=base, b=observer, autojunk=False)
    for ordinal, (tag, i1, i2, j1, j2) in enumerate(
        [op for op in matcher.get_opcodes() if op[0] != "equal"], start=1
    ):
        desired = observer[j1:j2]
        if desired and not occurrences(final, desired):
            raise SystemExit(f"{path}: Observer postcondition missing for edit {ordinal} {tag}: {compact(desired)!r}")


def main() -> int:
    compose_dart_contracts()
    for path in PATHS:
        replay_path(path)
    print(f"Observer semantic replay: PASS files={len(PATHS) + 1}", flush=True)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
