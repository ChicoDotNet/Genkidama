#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import re
import sys
from collections import Counter
from pathlib import Path
from urllib.parse import unquote

ROOT = Path(__file__).resolve().parents[2]
EXPECTED_PATTERN_COUNT = 52
AUXILIARY_WIKI_FILES = {"README.md", "Home.md"}

REQUIRED_PAGE_HEADINGS = (
    "En una frase",
    "El problema",
    "Fuerzas que compiten",
    "La solución",
    "Participantes y responsabilidades",
    "Cómo funciona",
    "Diagrama",
    "Ejemplo mínimo",
    "Aplicación real",
    "En Genkidama",
    "Cuándo usarlo",
    "Cuándo no usarlo",
    "Consecuencias y trade-offs",
    "Patrones relacionados",
    "Errores comunes y confusiones",
    "Cómo comprobar una implementación",
    "Implementaciones por lenguaje",
    "Comprueba que lo entendiste",
    "Resumen",
    "Referencias",
)

README_REQUIRED_SPANISH_HEADINGS = (
    "Cómo usar este catálogo",
    "Mapa de relaciones",
    "Constelaciones comunes de patrones",
    "Catálogo por familia",
    "Regla de mantenimiento",
)

MANUAL_REVIEW_CRITERIA = (
    "correctness of pattern intent and family",
    "meaningfulness of design forces and trade-offs",
    "accuracy of related-pattern distinctions",
    "idiomaticity of every applicable-language implementation",
    "technical defensibility of every N/A classification",
    "factual accuracy of the En Genkidama section",
    "realism and decision usefulness of applications and rejection guidance",
    "behavioral adequacy of implementation validation evidence",
    "quality of comprehension questions",
    "reference quality and copyright-safe paraphrasing",
)

MARKDOWN_LINK_RE = re.compile(r"(?<!!)\[[^\]]+\]\(([^)]+)\)")
STATE_RE = re.compile(r"\*\*Estado:\*\*\s*`([^`]+)`")
COUNTER_RE = re.compile(r"\*\*Implementaciones de lenguaje:\*\*\s*`(\d+)\s*/\s*(\d+)`")
FORBIDDEN_DEBT_RE = re.compile(r"\b(?:TODO|TBD|PLACEHOLDER)\b")
EXTERNAL_SCHEME_RE = re.compile(r"^[a-zA-Z][a-zA-Z0-9+.-]*:")
HEADING_RE = re.compile(r"^#{1,6}\s+(.+?)\s*#*\s*$", re.MULTILINE)
EXPLICIT_ANCHOR_RE = re.compile(r"<a\s+(?:[^>]*?\s)?id=[\"']([^\"']+)[\"'][^>]*>", re.IGNORECASE)


def _read(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def _link_destination(raw: str) -> str:
    value = raw.strip()
    if value.startswith("<") and ">" in value:
        return value[1:value.index(">")]
    return value.split(maxsplit=1)[0] if value else value


def _split_destination(destination: str) -> tuple[str, str | None]:
    raw_path, separator, raw_fragment = destination.partition("#")
    path_part = unquote(raw_path)
    fragment = unquote(raw_fragment) if separator else None
    return path_part, fragment


def markdown_links(text: str) -> list[str]:
    return [_link_destination(match.group(1)) for match in MARKDOWN_LINK_RE.finditer(text)]


def _github_heading_slug(heading: str) -> str:
    value = re.sub(r"<[^>]+>", "", heading)
    value = re.sub(r"[`*_~]", "", value).strip().lower()
    value = re.sub(r"[^\w\- ]", "", value, flags=re.UNICODE)
    return re.sub(r"\s+", "-", value)


def markdown_anchors(text: str) -> set[str]:
    anchors = {match.group(1) for match in EXPLICIT_ANCHOR_RE.finditer(text)}
    seen: Counter[str] = Counter()
    for match in HEADING_RE.finditer(text):
        base = _github_heading_slug(match.group(1))
        if not base:
            continue
        suffix = seen[base]
        anchors.add(base if suffix == 0 else f"{base}-{suffix}")
        seen[base] += 1
    return anchors


def catalog_targets(readme_text: str) -> list[str]:
    targets: list[str] = []
    for destination in markdown_links(readme_text):
        path_part, _ = _split_destination(destination)
        if not path_part or EXTERNAL_SCHEME_RE.match(path_part):
            continue
        candidate = Path(path_part)
        if candidate.parent == Path(".") and candidate.suffix.lower() == ".md" and candidate.name not in AUXILIARY_WIKI_FILES:
            targets.append(candidate.name)
    return targets


def _broken_links(source: Path, text: str, root: Path) -> list[str]:
    broken: list[str] = []
    seen: set[str] = set()
    for destination in markdown_links(text):
        if not destination or EXTERNAL_SCHEME_RE.match(destination):
            continue
        path_part, fragment = _split_destination(destination)
        if path_part.startswith("/"):
            resolved = root / path_part.lstrip("/")
        elif path_part:
            resolved = source.parent / path_part
        else:
            resolved = source

        is_broken = not resolved.exists()
        if not is_broken and fragment and resolved.suffix.lower() == ".md":
            is_broken = fragment not in markdown_anchors(_read(resolved))

        if is_broken and destination not in seen:
            seen.add(destination)
            broken.append(destination)
    return broken


def audit(root: Path = ROOT) -> dict[str, object]:
    root = root.resolve()
    wiki = root / "wiki"
    readme = wiki / "README.md"
    if not readme.exists():
        raise FileNotFoundError(f"Missing wiki catalog: {readme}")

    debt: list[dict[str, str]] = []

    def add(code: str, path: str, detail: str) -> None:
        debt.append({"code": code, "path": path, "detail": detail})

    readme_text = _read(readme)
    targets = catalog_targets(readme_text)
    counts = Counter(targets)
    unique_targets = sorted(counts)

    if len(unique_targets) != EXPECTED_PATTERN_COUNT:
        add(
            "CATALOG_COUNT",
            "wiki/README.md",
            f"expected {EXPECTED_PATTERN_COUNT} unique pattern targets, found {len(unique_targets)}",
        )

    for target, count in sorted(counts.items()):
        if count > 1:
            add("CATALOG_DUPLICATE", "wiki/README.md", f"{target} appears {count} times")

    pattern_files = sorted(
        path.name for path in wiki.glob("*.md") if path.name not in AUXILIARY_WIKI_FILES
    )
    target_set = set(unique_targets)
    file_set = set(pattern_files)

    for target in sorted(target_set - file_set):
        add("CATALOG_MISSING_PAGE", f"wiki/{target}", "catalog target does not exist")
    for filename in sorted(file_set - target_set):
        add("CATALOG_UNLISTED_PAGE", f"wiki/{filename}", "pattern page is not listed in catalog")

    for heading in README_REQUIRED_SPANISH_HEADINGS:
        if f"## {heading}" not in readme_text:
            add("README_SPANISH_HEADING_MISSING", "wiki/README.md", heading)

    for destination in _broken_links(readme, readme_text, root):
        add("BROKEN_LINK", "wiki/README.md", destination)

    empty_catalog_pages = 0
    validated_pages = 0
    in_progress_pages = 0
    nonempty_pages = 0
    pages_missing_sections: set[str] = set()

    for target in unique_targets:
        page = wiki / target
        if not page.exists():
            continue
        text = _read(page)
        relative = page.relative_to(root).as_posix()
        if not text.strip():
            empty_catalog_pages += 1
            add("PAGE_EMPTY", relative, "catalog target is empty")
            continue

        nonempty_pages += 1
        state_match = STATE_RE.search(text)
        state = state_match.group(1).strip() if state_match else None
        if state == "validated":
            validated_pages += 1
        elif state == "in-progress":
            in_progress_pages += 1
        else:
            add("PAGE_STATE_INVALID", relative, f"expected in-progress or validated, found {state!r}")

        missing_sections = [heading for heading in REQUIRED_PAGE_HEADINGS if f"## {heading}" not in text]
        if missing_sections:
            pages_missing_sections.add(relative)
            for heading in missing_sections:
                add("PAGE_SECTION_MISSING", relative, heading)

        if "```mermaid" not in text:
            add("PAGE_MERMAID_MISSING", relative, "completed KB-006 structure requires a Mermaid diagram")

        counter_match = COUNTER_RE.search(text)
        if not counter_match:
            add("PAGE_COUNTER_MISSING", relative, "missing implemented/applicable language counter")
        elif state == "validated":
            implemented = int(counter_match.group(1))
            applicable = int(counter_match.group(2))
            if implemented != applicable:
                add(
                    "VALIDATED_COUNTER_MISMATCH",
                    relative,
                    f"validated page reports {implemented}/{applicable}",
                )

        if state == "validated":
            marker = FORBIDDEN_DEBT_RE.search(text)
            if marker:
                add("VALIDATED_DEBT_MARKER", relative, marker.group(0))

        for destination in _broken_links(page, text, root):
            add("BROKEN_LINK", relative, destination)

    for auxiliary in sorted(AUXILIARY_WIKI_FILES - {"README.md"}):
        path = wiki / auxiliary
        if path.exists() and not _read(path).strip():
            add("AUXILIARY_PAGE_EMPTY", path.relative_to(root).as_posix(), "auxiliary wiki page is empty")

    by_code = Counter(item["code"] for item in debt)
    summary = {
        "expected_patterns": EXPECTED_PATTERN_COUNT,
        "catalog_entries": len(targets),
        "unique_catalog_targets": len(unique_targets),
        "pattern_files": len(pattern_files),
        "nonempty_catalog_pages": nonempty_pages,
        "empty_catalog_pages": empty_catalog_pages,
        "validated_pages": validated_pages,
        "in_progress_pages": in_progress_pages,
        "pages_missing_required_sections": len(pages_missing_sections),
        "broken_links": by_code["BROKEN_LINK"],
        "readme_spanish_headings_missing": by_code["README_SPANISH_HEADING_MISSING"],
        "validated_counter_mismatches": by_code["VALIDATED_COUNTER_MISMATCH"],
        "debt_items": len(debt),
    }

    return {
        "standard": "KB-006",
        "status": "GREEN" if not debt else "RED",
        "summary": summary,
        "debt_by_code": dict(sorted(by_code.items())),
        "debt": debt,
        "manual_review_required": list(MANUAL_REVIEW_CRITERIA),
    }


def print_human(result: dict[str, object]) -> None:
    summary = result["summary"]
    assert isinstance(summary, dict)
    print(f"KB-006 audit: {result['status']}")
    print(
        "Catalog: "
        f"{summary['unique_catalog_targets']}/{summary['expected_patterns']} targets; "
        f"{summary['nonempty_catalog_pages']} non-empty; "
        f"{summary['empty_catalog_pages']} empty; "
        f"{summary['validated_pages']} validated; "
        f"{summary['in_progress_pages']} in-progress"
    )
    print(
        "Debt: "
        f"{summary['debt_items']} machine-detectable items; "
        f"{summary['pages_missing_required_sections']} pages missing canonical sections; "
        f"{summary['broken_links']} broken relative links; "
        f"{summary['readme_spanish_headings_missing']} Spanish catalog headings missing"
    )
    print("KB006_AUDIT " + json.dumps(result, ensure_ascii=False, sort_keys=True))


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Audit the Genkidama wiki against machine-checkable KB-006 contracts.")
    parser.add_argument(
        "mode",
        nargs="?",
        choices=("audit", "validate"),
        default="audit",
        help="audit reports debt and exits 0; validate exits non-zero while debt remains",
    )
    parser.add_argument("--repo-root", help="Repository root; defaults to the current Genkidama checkout.")
    parser.add_argument("--json", action="store_true", help="Print only JSON output.")
    args = parser.parse_args(argv)

    root = Path(args.repo_root).resolve() if args.repo_root else ROOT
    result = audit(root)
    if args.json:
        print(json.dumps(result, ensure_ascii=False, sort_keys=True))
    else:
        print_human(result)
    return 1 if args.mode == "validate" and result["status"] != "GREEN" else 0


if __name__ == "__main__":
    raise SystemExit(main())
