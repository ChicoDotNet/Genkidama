from patterns.monitor_object import CounterMonitor
from std.testing import assert_equal, TestSuite


def test_guarded_state_changes_through_monitor_methods() raises:
    var counter = CounterMonitor()
    counter.increment()
    counter.increment(4)
    assert_equal(counter.current(), 5)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
