from __future__ import annotations

import json
import sys
import tempfile
import unittest
from pathlib import Path

CI_DIR = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(CI_DIR))

import engine  # noqa: E402


class RegistryTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.registry = engine.load_registry()

    def test_every_family_surface_points_to_a_target(self) -> None:
        targets = self.registry["targets"]
        for family, surfaces in self.registry["families"].items():
            for surface, target in surfaces.items():
                with self.subTest(family=family, surface=surface):
                    self.assertIn(target, targets)

    def test_full_paths_force_every_gate_and_family(self) -> None:
        result = engine.classify_paths(["eng/ci/engine.py"], self.registry)
        self.assertTrue(result["full"])
        self.assertTrue(result["product"])
        self.assertTrue(result["quality"])
        self.assertEqual(result["polyglot"], sorted(self.registry["families"]))

    def test_product_change_does_not_force_polyglot(self) -> None:
        result = engine.classify_paths(["src/Genkidama.Cli/Program.cs"], self.registry)
        self.assertTrue(result["product"])
        self.assertFalse(result["quality"])
        self.assertFalse(result["full"])
        self.assertEqual(result["polyglot"], [])

    def test_learn_change_reports_language(self) -> None:
        result = engine.classify_paths(["learn/es/rust/app/src/main.rs"], self.registry)
        self.assertTrue(result["quality"])
        self.assertEqual(result["learn_languages"], ["rust"])
        self.assertFalse(result["full"])

    def test_beam_pattern_change_selects_beam_only(self) -> None:
        result = engine.classify_paths(
            ["src/Functional/Elixir/patterns/enterprise_adapter.exs"],
            self.registry,
        )
        self.assertEqual(result["polyglot"], ["beam"])
        self.assertFalse(result["full"])

    def test_functional_pattern_changes_select_functional_only(self) -> None:
        paths = [
            "src/Functional/OCaml/patterns/enterprise_adapter.ml",
            "src/Functional/CommonLisp/patterns/enterprise_adapter.lisp",
            "src/Functional/Prolog/patterns/enterprise_adapter.pl",
        ]
        for path in paths:
            with self.subTest(path=path):
                result = engine.classify_paths([path], self.registry)
                self.assertEqual(result["polyglot"], ["functional"])
                self.assertFalse(result["full"])

    def test_data_shell_pattern_changes_select_data_shell_only(self) -> None:
        paths = [
            "src/DataScience/R/patterns/enterprise_adapter.R",
            "src/DataScience/Octave/patterns/enterprise_adapter.m",
            "src/Scripting/PowerShell/patterns/enterprise_adapter.ps1",
        ]
        for path in paths:
            with self.subTest(path=path):
                result = engine.classify_paths([path], self.registry)
                self.assertEqual(result["polyglot"], ["data-shell"])
                self.assertFalse(result["full"])

    def test_portable_functional_cohort_is_no_longer_a_family(self) -> None:
        self.assertNotIn("portable-functional", self.registry["families"])
        self.assertNotIn("patterns-portable-functional-507", self.registry["targets"])

    def test_unknown_path_fails_safe_to_full(self) -> None:
        result = engine.classify_paths(["new-surface/contract.txt"], self.registry)
        self.assertTrue(result["full"])
        self.assertEqual(result["unknown_paths"], ["new-surface/contract.txt"])


class RunnerTests(unittest.TestCase):
    def test_runner_is_fail_closed(self) -> None:
        registry = {
            "schema_version": 1,
            "families": {},
            "targets": {
                "probe": {
                    "setup": [],
                    "validation": [
                        {"label": "fail", "argv": [sys.executable, "-c", "raise SystemExit(7)"]},
                        {"label": "must-not-run", "argv": [sys.executable, "-c", "raise SystemExit(99)"]}
                    ]
                }
            }
        }
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            (root / "Genkidama.slnx").write_text("<Solution />", encoding="utf-8")
            outcome = engine.run_target("probe", root=root, registry=registry)
        self.assertEqual(outcome.status, "failed")
        self.assertEqual(outcome.exit_code, 7)

    def test_runner_appends_machine_readable_telemetry(self) -> None:
        registry = {
            "schema_version": 1,
            "families": {},
            "targets": {
                "probe": {
                    "setup": [{"label": "setup", "argv": [sys.executable, "-c", "pass"]}],
                    "validation": [{"label": "validate", "argv": [sys.executable, "-c", "pass"]}]
                }
            }
        }
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            (root / "Genkidama.slnx").write_text("<Solution />", encoding="utf-8")
            telemetry = root / "telemetry.jsonl"
            outcome = engine.run_target("probe", root=root, registry=registry, telemetry_file=str(telemetry))
            payload = json.loads(telemetry.read_text(encoding="utf-8").strip())
        self.assertEqual(outcome.exit_code, 0)
        self.assertEqual(payload["target"], "probe")
        self.assertEqual(payload["status"], "passed")
        self.assertIn("setup_seconds", payload)
        self.assertIn("validation_seconds", payload)
        self.assertIn("total_seconds", payload)


if __name__ == "__main__":
    unittest.main()
