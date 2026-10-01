from patterns.enterprise_facade import run
from std.testing import assert_equal, TestSuite


def test_enterprise_facade_contract() raises:
    assert_equal(run(), True)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
