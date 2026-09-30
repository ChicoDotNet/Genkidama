from patterns.peer_to_peer import run
from std.testing import assert_equal, TestSuite


def test_peer_to_peer_contract() raises:
    assert_equal(run(), True)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
