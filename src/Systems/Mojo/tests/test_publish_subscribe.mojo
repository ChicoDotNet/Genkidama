from patterns.publish_subscribe import run
from std.testing import assert_equal, TestSuite


def test_publish_subscribe_contract() raises:
    assert_equal(run(), True)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
