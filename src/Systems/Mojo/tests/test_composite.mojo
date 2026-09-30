from patterns.composite import run
from std.testing import assert_equal, TestSuite


def test_composite_contract() raises:
    assert_equal(run(), True)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
