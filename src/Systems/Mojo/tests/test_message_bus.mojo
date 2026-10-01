from patterns.message_bus import run
from std.testing import assert_equal, TestSuite


def test_message_bus_contract() raises:
    assert_equal(run(), True)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
