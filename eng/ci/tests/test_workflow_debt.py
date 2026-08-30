from __future__ import annotations

import sys
import unittest
from pathlib import Path
from unittest import mock

CI_DIR = Path(__file__).resolve().parents[1]
if str(CI_DIR) not in sys.path:
    sys.path.insert(0, str(CI_DIR))

import workflow_debt as wd


class WorkflowInventoryTests(unittest.TestCase):
    def test_current_inventory_is_exactly_the_three_consolidated_gates(self) -> None:
        stats = wd.validate()
        self.assertEqual(stats["workflow_count"], 3)
        self.assertEqual(
            set(stats["workflows"]),
            {"ci.yml", "quality.yml", "polyglot.yml"},
        )
        self.assertEqual(stats["status"], "consolidated")

    def test_guardrail_rejects_workflow_sprawl(self) -> None:
        drifted = set(wd.ALLOWED_WORKFLOWS) | {"pattern-regression.yml"}
        with mock.patch.object(wd, "actual_workflows", return_value=drifted):
            with self.assertRaises(wd.WorkflowInventoryError):
                wd.validate()

    def test_guardrail_rejects_missing_consolidated_gate(self) -> None:
        incomplete = set(wd.ALLOWED_WORKFLOWS) - {"polyglot.yml"}
        with mock.patch.object(wd, "actual_workflows", return_value=incomplete):
            with self.assertRaises(wd.WorkflowInventoryError):
                wd.validate()


if __name__ == "__main__":
    unittest.main()
