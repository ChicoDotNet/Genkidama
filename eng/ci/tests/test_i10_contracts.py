from __future__ import annotations

import sys
import unittest
from pathlib import Path

CI_DIR = Path(__file__).resolve().parents[1]
ROOT = CI_DIR.parents[1]
sys.path.insert(0, str(CI_DIR))
sys.path.insert(0, str(CI_DIR / "adapters"))

import debt_contracts  # noqa: E402
import early_patterns  # noqa: E402
import early_patterns_runner  # noqa: E402
import engine  # noqa: E402


class I10RegistryCoverageTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.registry = engine.load_registry(CI_DIR / "registry.json")

    def test_extracted_family_surfaces_have_local_contracts(self) -> None:
        extracted = {"git", "web", "scripting", "gnu", "dart", "swift", "longtail", "platform"}
        for family in extracted:
            for surface in self.registry["families"][family]:
                with self.subTest(family=family, surface=surface):
                    self.assertIn((family, surface), debt_contracts.CONTRACTS)

    def test_every_pattern_family_has_pre_cor_runner(self) -> None:
        pattern_families = {
            family
            for family, surfaces in self.registry["families"].items()
            if "patterns" in surfaces
        }
        self.assertEqual(pattern_families, set(early_patterns.VALIDATORS))

    def test_pre_cor_contract_set_is_the_historical_thirteen(self) -> None:
        expected = {
            "abstract_factory",
            "adapter",
            "bridge",
            "builder",
            "chain_of_responsibility",
            "composite",
            "decorator",
            "facade",
            "factory_method",
            "flyweight",
            "prototype",
            "proxy",
            "singleton",
        }
        self.assertEqual(set(early_patterns.PATTERN_MARKERS), expected)

    def test_pre_cor_filename_normalization_is_explicit(self) -> None:
        cases = {
            "Example1.pas": "abstract_factory",
            "AdapterExample.cs": "adapter",
            "chain_of_responsibility.erl": "chain_of_responsibility",
            "FactoryMethodExample.vb": "factory_method",
            "PrototypeExample.kt": "prototype",
        }
        for filename, expected in cases.items():
            with self.subTest(filename=filename):
                self.assertEqual(early_patterns.pattern_key(Path(filename)), expected)

    def test_remaining_legacy_paths_route_to_clean_families(self) -> None:
        cases = {
            "learn/es/git/lessons/01.md": "git",
            "learn/es/javascript/app/server.js": "web",
            "learn/es/python/app/pyproject.toml": "scripting",
            "learn/es/cobol/app/tests/smoke.sh": "gnu",
            "learn/es/dart/app/pubspec.yaml": "dart",
            "learn/es/swift/app/Package.swift": "swift",
            "src/Functional/Haskell/PatternSweep.hs": "longtail",
            "src/DataScience/MATLAB/validate_pattern_sweep.m": "platform",
            "src/Functional/Scala/PatternSweep.scala": "jvm",
            "src/Enterprise/C#/AdapterExample.cs": "dotnet",
            "src/Other/Rockstar/proxy.rock": "platform",
        }
        for path, family in cases.items():
            with self.subTest(path=path):
                result = engine.classify_paths([path], self.registry)
                self.assertEqual(result["polyglot"], [family])
                self.assertFalse(result["full"])


class I10LegacyEntrypointTests(unittest.TestCase):
    def test_java_main_class_uses_compilation_unit_not_nested_helper(self) -> None:
        source = ROOT / "src/Enterprise/Java/ChainOfResponsibilityExample.java"
        self.assertEqual(
            early_patterns_runner.java_main_class(source.read_text(encoding="utf-8"), source.name),
            "ChainOfResponsibilityExample",
        )

    def test_prolog_entry_goal_preserves_declared_main(self) -> None:
        source = ROOT / "src/Niche/Prolog/facade.pl"
        self.assertEqual(
            early_patterns_runner.prolog_entry_goal(source.read_text(encoding="utf-8"), source.name),
            "main",
        )

    def test_vba_entrypoint_is_semantic_not_named_usage(self) -> None:
        source = ROOT / "src/Shell/VBA/DecoratorExample.bas"
        text = source.read_text(encoding="utf-8")
        self.assertNotIn("Usage", text)
        self.assertTrue(early_patterns_runner.vba_has_public_entrypoint(text))

    def test_fortran_abstract_factory_source_contract_allows_composed_output(self) -> None:
        source = ROOT / "src/Historical/Fortran/example1.f90"
        text = source.read_text(encoding="utf-8")
        self.assertNotIn("Dark Button", text)
        early_patterns_runner.assert_abstract_factory_source_contract("fortran", source)

    def test_legacy_output_normalization_is_case_and_spacing_tolerant(self) -> None:
        self.assertEqual(
            early_patterns_runner.normalize_contract_text("same=TRUE\ncount= 1\n"),
            "same=true\ncount=1",
        )

    def test_legacy_output_normalization_ignores_field_separator_spacing(self) -> None:
        self.assertEqual(
            early_patterns_runner.normalize_contract_text("styles=2;shared=true ;text=ABC\n"),
            "styles=2;shared=true;text=abc",
        )

    def test_rockstar_singleton_accepts_historical_raw_count_line(self) -> None:
        early_patterns_runner.assert_legacy_output("Rockstar", "singleton", "same=true\n1\n")

    def test_zig_stdout_contract_is_015_and_016_compatible(self) -> None:
        for filename in ("facade.zig", "flyweight.zig", "proxy.zig"):
            with self.subTest(filename=filename):
                text = (ROOT / "src/Systems/Zig" / filename).read_text(encoding="utf-8")
                self.assertIn("std.os.linux.syscall3(.write", text)
                self.assertIn("@intFromPtr(bytes.ptr)", text)
                self.assertIn("if (written != bytes.len)", text)
                self.assertNotIn("std.posix.write", text)
                self.assertNotIn("std.fs.File.stdout()", text)
                self.assertNotIn("std.process.Init", text)

    def test_objective_c_gnustep_properties_are_explicit(self) -> None:
        cases = {
            "adapter.m": ("LegacyFahrenheitSensor *_adaptee;", "@synthesize adaptee = _adaptee;"),
            "proxy.m": (
                "NSInteger _fetches;",
                "@synthesize fetches = _fetches;",
                "RemoteDocumentStore *_backend;",
                "NSMutableDictionary<NSNumber *, NSString *> *_cache;",
                "NSInteger _backendCreations;",
                "@synthesize backend = _backend;",
                "@synthesize cache = _cache;",
                "@synthesize backendCreations = _backendCreations;",
            ),
        }
        for filename, markers in cases.items():
            with self.subTest(filename=filename):
                text = (ROOT / "src/Systems/Objective-C" / filename).read_text(encoding="utf-8")
                for marker in markers:
                    self.assertIn(marker, text)

    def test_longtail_toolchain_script_pins_zig_and_nim(self) -> None:
        text = (CI_DIR / "toolchains/setup_longtail.sh").read_text(encoding="utf-8")
        self.assertIn('ZIG_VERSION="0.16.0"', text)
        self.assertIn(
            'ZIG_SHA256="70e49664a74374b48b51e6f3fdfbf437f6395d42509050588bd49abe52ba3d00"',
            text,
        )
        self.assertIn('NIM_VERSION="2.2.10"', text)
        self.assertNotIn("choosenim stable", text)
        self.assertNotIn("download/index.json", text)

    def test_polyglot_longtail_workflow_pins_runtime_versions(self) -> None:
        text = (ROOT / ".github/workflows/polyglot.yml").read_text(encoding="utf-8")
        self.assertIn("ghc-version: '9.14.1'", text)
        self.assertIn("cabal-version: '3.16.1.0'", text)
        self.assertIn("crystal: '1.21.0'", text)
        self.assertIn("version: '1.12.7'", text)
        self.assertIn("run: bash eng/ci/toolchains/setup_longtail.sh", text)


if __name__ == "__main__":
    unittest.main()
