from __future__ import annotations

import sqlite3
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]


class StrategySqlContractTests(unittest.TestCase):
    def setUp(self) -> None:
        self.connection = sqlite3.connect(":memory:")
        source = (ROOT / "src/Data/SQL/strategy.sql").read_text(encoding="utf-8")
        self.connection.executescript(source)

    def tearDown(self) -> None:
        self.connection.close()

    def price(self) -> tuple[str, int]:
        row = self.connection.execute(
            "SELECT strategy_name, final_price FROM pricing_result WHERE slot = 'checkout'"
        ).fetchone()
        self.assertIsNotNone(row)
        return row

    def test_context_substitutes_pricing_policies_without_changing_evaluation(self) -> None:
        self.assertEqual(self.price(), ("regular", 100))

        self.connection.execute(
            "UPDATE pricing_context SET strategy_name = 'vip' WHERE slot = 'checkout'"
        )
        self.assertEqual(self.price(), ("vip", 80))

        self.connection.execute(
            "UPDATE pricing_context SET strategy_name = 'campaign' WHERE slot = 'checkout'"
        )
        self.assertEqual(self.price(), ("campaign", 75))

        self.connection.execute(
            "UPDATE pricing_request SET base_price = 80 WHERE slot = 'checkout'"
        )
        self.assertEqual(self.price(), ("campaign", 80))

    def test_unknown_strategy_is_rejected_by_context_contract(self) -> None:
        with self.assertRaises(sqlite3.IntegrityError):
            self.connection.execute(
                "UPDATE pricing_context SET strategy_name = 'missing' WHERE slot = 'checkout'"
            )


if __name__ == "__main__":
    unittest.main()
