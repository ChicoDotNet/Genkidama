from patterns.decorator import run
from std.testing import assert_equal, TestSuite


def test_decorator_contract() raises:
    assert_equal(run(), True)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
