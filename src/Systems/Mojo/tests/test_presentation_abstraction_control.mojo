from patterns.presentation_abstraction_control import run
from std.testing import assert_equal, TestSuite


def test_presentation_abstraction_control_contract() raises:
    assert_equal(run(), True)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
