from __future__ import annotations

import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


class StrategyVbaContractTests(unittest.TestCase):
    def test_vba_strategy_has_addressable_contract_and_two_substitutable_policies(self) -> None:
        module = (ROOT / "src/Shell/VBA/strategy.bas").read_text(encoding="utf-8")
        interface = (ROOT / "src/Shell/VBA/IStrategyPricing.cls").read_text(encoding="utf-8")
        regular = (ROOT / "src/Shell/VBA/StrategyRegularPricing.cls").read_text(encoding="utf-8")
        vip = (ROOT / "src/Shell/VBA/StrategyVipPricing.cls").read_text(encoding="utf-8")

        self.assertRegex(module, r"(?im)^Option Explicit$")
        self.assertRegex(module, r"(?im)^Public Sub RunStrategyExample\(\)")
        self.assertIn("ApplyPricing(100, regular)", module)
        self.assertIn("ApplyPricing(100, vip)", module)
        self.assertIn("regularPrice <> 100", module)
        self.assertIn("vipPrice <> 80", module)

        self.assertRegex(interface, r"(?im)^Public Function Price\(ByVal amount As Currency\) As Currency$")
        self.assertRegex(regular, r"(?im)^Implements IStrategyPricing$")
        self.assertRegex(vip, r"(?im)^Implements IStrategyPricing$")
        self.assertIn("IStrategyPricing_Price = amount", regular)
        self.assertIn("IStrategyPricing_Price = amount * 0.8", vip)

        self.assertIsNotNone(re.search(r"(?im)^\s*Public\s+(?:Sub|Function)\s+[A-Za-z_][A-Za-z0-9_]*\b", module))


if __name__ == "__main__":
    unittest.main()
