from patterns.distributed_proxy import run
from std.testing import assert_equal, TestSuite


def test_distributed_proxy_contract() raises:
    assert_equal(run(), True)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
