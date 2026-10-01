from patterns.factory_method import run
from std.testing import assert_equal, TestSuite


def test_factory_method_contract() raises:
    assert_equal(run(), True)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
