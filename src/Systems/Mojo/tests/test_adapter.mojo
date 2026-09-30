from patterns.adapter import CelsiusAdapter, LegacyFahrenheitSensor
from std.testing import assert_equal, TestSuite


def test_freezing_point_is_adapted() raises:
    var adapter = CelsiusAdapter(LegacyFahrenheitSensor(32))
    assert_equal(adapter.celsius(), 0)


def test_boiling_point_is_adapted() raises:
    var adapter = CelsiusAdapter(LegacyFahrenheitSensor(212))
    assert_equal(adapter.celsius(), 100)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
