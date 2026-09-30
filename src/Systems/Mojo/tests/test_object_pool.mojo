from patterns.object_pool import run
from std.testing import assert_equal, TestSuite


def test_object_pool_contract() raises:
    assert_equal(run(), True)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
