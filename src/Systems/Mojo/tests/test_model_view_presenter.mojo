from patterns.model_view_presenter import run
from std.testing import assert_equal, TestSuite


def test_model_view_presenter_contract() raises:
    assert_equal(run(), True)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
