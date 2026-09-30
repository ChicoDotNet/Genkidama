from patterns.client_server import run
from std.testing import assert_equal, TestSuite


def test_client_server_contract() raises:
    assert_equal(run(), True)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
