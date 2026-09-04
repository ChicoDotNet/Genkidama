from __future__ import annotations

import shutil
import subprocess
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
SOURCE = ROOT / "src/Scripting/Perl/observer.pl"


class ObserverPerlContractTests(unittest.TestCase):
    def test_perl_observer_compiles_and_runs_behavioral_contract(self) -> None:
        perl = shutil.which("perl")
        self.assertIsNotNone(perl, "Perl runtime is required for the Observer contract")

        syntax = subprocess.run(
            [perl, "-c", str(SOURCE)],
            cwd=ROOT,
            check=False,
            capture_output=True,
            text=True,
        )
        self.assertEqual(0, syntax.returncode, syntax.stderr)

        run = subprocess.run(
            [perl, str(SOURCE)],
            cwd=ROOT,
            check=False,
            capture_output=True,
            text=True,
        )
        self.assertEqual(0, run.returncode, run.stderr)
        self.assertIn("OBSERVER_PERL_OK", run.stdout)


if __name__ == "__main__":
    unittest.main()
