from __future__ import annotations

import sys
import unittest
from pathlib import Path

CI_DIR = Path(__file__).resolve().parents[1]
if str(CI_DIR) not in sys.path:
    sys.path.insert(0, str(CI_DIR))

import workflow_debt as wd


class WorkflowDebtTests(unittest.TestCase):
    def test_frozen_baseline_digest_matches_ledger(self) -> None:
        data = wd._load(wd.LEDGER)
        names = data["baseline"]["legacy_names"]
        self.assertEqual(wd.baseline_digest(names), wd.FROZEN_BASELINE_SHA256)
        self.assertEqual(len(names), 73)

    def test_current_census_is_exhaustive_and_fail_closed(self) -> None:
        stats = wd.validate()
        self.assertEqual(stats.retained, 3)
        self.assertEqual(stats.legacy, 73)
        self.assertEqual(stats.present, 73)
        self.assertEqual(stats.retired, 0)
        self.assertEqual(stats.extracted, 20)
        self.assertEqual(stats.superseded, 34)
        self.assertEqual(stats.unresolved, 19)

    def test_purge_gate_rejects_unresolved_contracts(self) -> None:
        with self.assertRaises(wd.WorkflowDebtError):
            wd.validate(require_retirable=True)


if __name__ == "__main__":
    unittest.main()
