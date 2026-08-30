from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path

CI_DIR = Path(__file__).resolve().parents[1]
if str(CI_DIR) not in sys.path:
    sys.path.insert(0, str(CI_DIR))

import kb006_audit as kb


class Kb006AuditTests(unittest.TestCase):
    def _root(self) -> tuple[tempfile.TemporaryDirectory[str], Path]:
        temp = tempfile.TemporaryDirectory()
        root = Path(temp.name)
        (root / "wiki").mkdir()
        return temp, root

    def _spanish_readme(self, links: str) -> str:
        return f"""# Catálogo de patrones de diseño de Genkidama

## Cómo usar este catálogo
Texto.

## Mapa de relaciones
Texto.

## Constelaciones comunes de patrones
Texto.

## Catálogo por familia
{links}

## Regla de mantenimiento
Texto.
"""

    def _validated_page(self, extra: str = "") -> str:
        sections = "\n\n".join(f"## {heading}\nContenido verificable." for heading in kb.REQUIRED_PAGE_HEADINGS)
        return f"""# Sample

> **Familia:** Behavioral  
> **Intención:** Ejemplo.  
> **Estado:** `validated`  
> **Implementaciones de lenguaje:** `1/1`  
> **Cobertura de pruebas:** `N/A`  
> **Mapa:** [Volver](README.md)

{sections}

```mermaid
flowchart LR
  A --> B
```

{extra}
"""

    def _write_full_catalog(self, root: Path, page: str) -> Path:
        wiki = root / "wiki"
        links = "\n".join(f"- [Pattern {index}](Pattern{index}.md)" for index in range(kb.EXPECTED_PATTERN_COUNT))
        (wiki / "README.md").write_text(self._spanish_readme(links), encoding="utf-8")
        for index in range(kb.EXPECTED_PATTERN_COUNT):
            (wiki / f"Pattern{index}.md").write_text(page, encoding="utf-8")
        return wiki

    def test_green_fixture_has_no_machine_detectable_debt(self) -> None:
        temp, root = self._root()
        self.addCleanup(temp.cleanup)
        self._write_full_catalog(root, self._validated_page())

        result = kb.audit(root)

        self.assertEqual(result["status"], "GREEN")
        self.assertEqual(result["summary"]["debt_items"], 0)
        self.assertEqual(result["summary"]["validated_pages"], kb.EXPECTED_PATTERN_COUNT)

    def test_empty_catalog_target_is_reported_as_red(self) -> None:
        temp, root = self._root()
        self.addCleanup(temp.cleanup)
        wiki = self._write_full_catalog(root, self._validated_page())
        (wiki / "Pattern7.md").write_text("", encoding="utf-8")

        result = kb.audit(root)

        self.assertEqual(result["status"], "RED")
        self.assertEqual(result["summary"]["empty_catalog_pages"], 1)
        self.assertIn("PAGE_EMPTY", result["debt_by_code"])

    def test_validated_counter_mismatch_and_broken_link_are_detected(self) -> None:
        temp, root = self._root()
        self.addCleanup(temp.cleanup)
        healthy = self._validated_page()
        wiki = self._write_full_catalog(root, healthy)
        missing = "[Implementación](../src/missing/example.py)"
        drifted = healthy.replace("`1/1`", "`0/1`") + f"\n{missing}\n{missing}\n"
        (wiki / "Pattern9.md").write_text(drifted, encoding="utf-8")

        result = kb.audit(root)

        self.assertEqual(result["status"], "RED")
        self.assertEqual(result["summary"]["validated_counter_mismatches"], 1)
        self.assertEqual(result["summary"]["broken_links"], 1)

    def test_english_catalog_headings_are_explicit_debt(self) -> None:
        temp, root = self._root()
        self.addCleanup(temp.cleanup)
        wiki = self._write_full_catalog(root, self._validated_page())
        readme = (wiki / "README.md").read_text(encoding="utf-8")
        readme = readme.replace("## Cómo usar este catálogo", "## How to use this catalog")
        (wiki / "README.md").write_text(readme, encoding="utf-8")

        result = kb.audit(root)

        self.assertEqual(result["status"], "RED")
        self.assertEqual(result["summary"]["readme_spanish_headings_missing"], 1)

    def test_spanish_todo_word_is_not_a_debt_marker(self) -> None:
        temp, root = self._root()
        self.addCleanup(temp.cleanup)
        page = self._validated_page("Todo el comportamiento sigue siendo verificable.")
        self._write_full_catalog(root, page)

        result = kb.audit(root)

        self.assertEqual(result["status"], "GREEN")
        self.assertNotIn("VALIDATED_DEBT_MARKER", result["debt_by_code"])

    def test_url_encoded_hash_in_repository_path_is_not_an_anchor(self) -> None:
        temp, root = self._root()
        self.addCleanup(temp.cleanup)
        source = root / "src" / "Enterprise" / "C#"
        source.mkdir(parents=True)
        (source / "Example.cs").write_text("// example", encoding="utf-8")
        page = self._validated_page("[C#](../src/Enterprise/C%23/Example.cs)")
        self._write_full_catalog(root, page)

        result = kb.audit(root)

        self.assertEqual(result["status"], "GREEN")
        self.assertEqual(result["summary"]["broken_links"], 0)

    def test_markdown_anchor_must_resolve(self) -> None:
        temp, root = self._root()
        self.addCleanup(temp.cleanup)
        page = self._validated_page("[Mapa estable](README.md#relationship-map)")
        self._write_full_catalog(root, page)

        result = kb.audit(root)

        self.assertEqual(result["status"], "RED")
        self.assertEqual(result["summary"]["broken_links"], kb.EXPECTED_PATTERN_COUNT)


if __name__ == "__main__":
    unittest.main()
