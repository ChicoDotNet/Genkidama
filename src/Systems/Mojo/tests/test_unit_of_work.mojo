from patterns.unit_of_work import run
from std.testing import assert_equal, TestSuite


def test_unit_of_work_contract() raises:
    assert_equal(run(), True)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
