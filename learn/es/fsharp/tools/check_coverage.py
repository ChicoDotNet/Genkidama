from __future__ import annotations

import glob
import sys
import xml.etree.ElementTree as ET

paths = glob.glob("learn/es/fsharp/TestResults/**/coverage.cobertura.xml", recursive=True)
if not paths:
    print("coverage.cobertura.xml not found", file=sys.stderr)
    raise SystemExit(2)

root = ET.parse(paths[0]).getroot()
line_rate = float(root.attrib["line-rate"])
percent = line_rate * 100
print(f"F# QuoteRules line coverage: {percent:.2f}%")

if line_rate < 0.44:
    print("Coverage is below the Genkidama 44% floor.", file=sys.stderr)
    raise SystemExit(1)
