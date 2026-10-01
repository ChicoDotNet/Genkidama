from patterns.data_mapper import run
from std.testing import assert_equal, TestSuite


def test_data_mapper_contract() raises:
    assert_equal(run(), True)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
