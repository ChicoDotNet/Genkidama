#!/usr/bin/env python3
"""Validate Genkidama Learn metadata, structure and internal Markdown links."""

from __future__ import annotations

import re
import sys
from pathlib import Path
from urllib.parse import unquote

import yaml

ROOT = Path(__file__).resolve().parents[2]
LEARN = ROOT / "learn"
META = LEARN / "_meta"

REQUIRED_FOUNDATION = (
    LEARN / "README.md",
    META / "pedagogy.md",
    META / "course-spec.md",
    META / "authoring-guide.md",
    META / "roadmap.md",
    META / "decisions.md",
    META / "progress.yml",
    META / "catalog.yml",
)

EXPECTED_LOCALES = ["es", "en", "zh-Hans", "ja", "fr", "it", "pt-BR", "ru", "de"]
EXPECTED_PILOTS = ["csharp", "python", "javascript", "cobol", "solidity"]
TRANSVERSAL_COURSE_SLUGS = {"git"}
MARKDOWN_LINK = re.compile(r"\[[^\]]+\]\(([^)]+)\)")
LESSON_FILE = re.compile(r"^(?P<number>\d+)-.+\.md$")


def fail(errors: list[str], message: str) -> None:
    errors.append(message)


def load_yaml(path: Path) -> dict:
    with path.open(encoding="utf-8") as stream:
        value = yaml.safe_load(stream)
    if not isinstance(value, dict):
        raise ValueError(f"{path.relative_to(ROOT)} must contain a mapping")
    return value


def validate_foundation(errors: list[str]) -> None:
    for path in REQUIRED_FOUNDATION:
        if not path.is_file():
            fail(errors, f"missing required foundation file: {path.relative_to(ROOT)}")


def validate_catalog(errors: list[str]) -> tuple[dict, set[str]]:
    catalog = load_yaml(META / "catalog.yml")
    courses = catalog.get("courses")
    if not isinstance(courses, list):
        fail(errors, "catalog.yml: courses must be a list")
        return catalog, set()

    slugs = [course.get("slug") for course in courses if isinstance(course, dict)]
    unique_slugs = {slug for slug in slugs if isinstance(slug, str)}
    if len(slugs) != len(unique_slugs):
        fail(errors, "catalog.yml: course slugs must be unique")

    current = [c for c in courses if isinstance(c, dict) and c.get("v1_required") is True]
    additional = [c for c in courses if isinstance(c, dict) and c.get("v1_required") is False]

    if len(current) != 45:
        fail(errors, f"catalog.yml: expected 45 v1 courses, found {len(current)}")
    if len(additional) != 6:
        fail(errors, f"catalog.yml: expected 6 planned additional courses, found {len(additional)}")

    if catalog.get("source_locale") != "es":
        fail(errors, "catalog.yml: source_locale must be es")
    if catalog.get("translation_order") != EXPECTED_LOCALES:
        fail(errors, "catalog.yml: translation_order does not match the agreed order")
    if catalog.get("pilots") != EXPECTED_PILOTS:
        fail(errors, "catalog.yml: pilot order must be csharp, python, javascript, cobol, solidity")

    by_slug = {c.get("slug"): c for c in courses if isinstance(c, dict)}
    solidity = by_slug.get("solidity", {})
    if "FreelanceEscrow" not in str(solidity.get("canonical_app", "")):
        fail(errors, "catalog.yml: Solidity must preserve FreelanceEscrow as the canonical concept")

    rockstar = by_slug.get("rockstar", {})
    if not rockstar.get("market_note"):
        fail(errors, "catalog.yml: Rockstar must include an honest market_note")

    for course in courses:
        if not isinstance(course, dict):
            fail(errors, "catalog.yml: every course entry must be a mapping")
            continue
        for field in ("slug", "name", "status", "canonical_app"):
            if not course.get(field):
                fail(errors, f"catalog.yml: course entry missing {field}: {course!r}")

    return catalog, unique_slugs


def validate_progress(errors: list[str], catalog_slugs: set[str]) -> None:
    progress = load_yaml(META / "progress.yml")
    courses = progress.get("courses")
    if not isinstance(courses, dict):
        fail(errors, "progress.yml: courses must be a mapping")
        return

    unknown = set(courses) - catalog_slugs
    if unknown:
        fail(errors, f"progress.yml: unknown course slugs: {sorted(unknown)}")

    if len(courses) != 45:
        fail(errors, f"progress.yml: expected progress for 45 v1 courses, found {len(courses)}")

    for index, slug in enumerate(EXPECTED_PILOTS, start=1):
        item = courses.get(slug, {})
        if item.get("pilot_order") != index:
            fail(errors, f"progress.yml: {slug} must have pilot_order {index}")

    transversal = progress.get("transversal_courses")
    if not isinstance(transversal, dict):
        fail(errors, "progress.yml: transversal_courses must be a mapping")
        return

    if set(transversal) != TRANSVERSAL_COURSE_SLUGS:
        fail(
            errors,
            "progress.yml: transversal_courses must track exactly the configured transversal course slugs",
        )

    for slug in TRANSVERSAL_COURSE_SLUGS:
        item = transversal.get(slug, {})
        complete = item.get("lessons_complete")
        total = item.get("lessons_total")
        if not isinstance(complete, int) or not isinstance(total, int) or not 0 <= complete <= total:
            fail(errors, f"progress.yml: invalid lesson progress for transversal course {slug}")


def validate_course_directories(errors: list[str], catalog_slugs: set[str]) -> None:
    locale_root = LEARN / "es"
    if not locale_root.exists():
        return

    known_slugs = catalog_slugs | TRANSVERSAL_COURSE_SLUGS
    for course_dir in sorted(path for path in locale_root.iterdir() if path.is_dir()):
        slug = course_dir.name
        if slug not in known_slugs:
            fail(errors, f"unregistered course directory: {course_dir.relative_to(ROOT)}")

        readme = course_dir / "README.md"
        metadata = course_dir / "course.yml"
        if not readme.is_file():
            fail(errors, f"{slug}: README.md is required once a course directory exists")
        if not metadata.is_file():
            fail(errors, f"{slug}: course.yml is required once a course directory exists")
            continue

        course = load_yaml(metadata)
        if course.get("slug") != slug:
            fail(errors, f"{slug}: course.yml slug must match the directory name")
        if slug in TRANSVERSAL_COURSE_SLUGS and course.get("course_kind") != "transversal":
            fail(errors, f"{slug}: transversal course must declare course_kind: transversal")

        if course.get("status") == "complete":
            lessons_dir = course_dir / "lessons"
            lessons = sorted(lessons_dir.glob("*.md")) if lessons_dir.is_dir() else []
            if not 13 <= len(lessons) <= 22:
                fail(errors, f"{slug}: complete course must have 13–22 lesson Markdown files")
            for required in ("app", "exercises", "solutions"):
                if not (course_dir / required).exists():
                    fail(errors, f"{slug}: complete course is missing {required}/")


def clean_target(raw_target: str) -> str:
    target = raw_target.strip().split()[0]
    if target.startswith("<") and target.endswith(">"):
        target = target[1:-1]
    return unquote(target)


def local_markdown_targets(markdown: Path) -> set[Path]:
    targets: set[Path] = set()
    text = markdown.read_text(encoding="utf-8")
    for raw_target in MARKDOWN_LINK.findall(text):
        target = clean_target(raw_target)
        if not target or target.startswith(("#", "http://", "https://", "mailto:")):
            continue
        target = target.split("#", 1)[0]
        if target:
            targets.add((markdown.parent / target).resolve())
    return targets


def validate_lesson_navigation(errors: list[str]) -> None:
    locale_root = LEARN / "es"
    if not locale_root.exists():
        return

    for course_dir in sorted(path for path in locale_root.iterdir() if path.is_dir()):
        lessons_dir = course_dir / "lessons"
        if not lessons_dir.is_dir():
            continue

        numbered_lessons: list[tuple[int, Path]] = []
        for lesson in lessons_dir.glob("*.md"):
            match = LESSON_FILE.match(lesson.name)
            if match:
                numbered_lessons.append((int(match.group("number")), lesson))
        numbered_lessons.sort(key=lambda item: item[0])

        for (current_number, current), (next_number, next_lesson) in zip(
            numbered_lessons, numbered_lessons[1:]
        ):
            if next_number != current_number + 1:
                continue
            if next_lesson.resolve() not in local_markdown_targets(current):
                fail(
                    errors,
                    f"{current.relative_to(ROOT)}: lesson {current_number:02d} must link to next lesson {next_lesson.name}",
                )


def validate_markdown_links(errors: list[str]) -> None:
    for markdown in sorted(LEARN.rglob("*.md")):
        text = markdown.read_text(encoding="utf-8")
        for raw_target in MARKDOWN_LINK.findall(text):
            target = clean_target(raw_target)
            if not target or target.startswith(("#", "http://", "https://", "mailto:")):
                continue
            target = target.split("#", 1)[0]
            if not target:
                continue
            resolved = (markdown.parent / target).resolve()
            try:
                resolved.relative_to(ROOT.resolve())
            except ValueError:
                fail(errors, f"{markdown.relative_to(ROOT)}: link escapes repository: {raw_target}")
                continue
            if not resolved.exists():
                fail(errors, f"{markdown.relative_to(ROOT)}: broken relative link: {raw_target}")


def main() -> int:
    errors: list[str] = []
    try:
        validate_foundation(errors)
        _, catalog_slugs = validate_catalog(errors)
        validate_progress(errors, catalog_slugs)
        validate_course_directories(errors, catalog_slugs)
        validate_lesson_navigation(errors)
        validate_markdown_links(errors)
    except (OSError, ValueError, yaml.YAMLError) as exc:
        fail(errors, str(exc))

    if errors:
        print("Genkidama Learn validation failed:", file=sys.stderr)
        for error in errors:
            print(f"- {error}", file=sys.stderr)
        return 1

    print("Genkidama Learn validation passed.")
    print("Validated 45 v1 language courses, 6 planned additions, transversal Git, metadata, lesson navigation and Markdown links.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
