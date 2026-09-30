from patterns.strategy import apply_strategy, double, square
from std.testing import assert_equal, TestSuite


def test_context_accepts_double_strategy() raises:
    assert_equal(apply_strategy(4, double), 8)


def test_context_accepts_square_strategy() raises:
    assert_equal(apply_strategy(4, square), 16)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
