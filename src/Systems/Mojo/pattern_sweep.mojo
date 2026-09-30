from patterns.adapter import CelsiusAdapter, LegacyFahrenheitSensor
from patterns.microkernel import Kernel, double_plugin, square_plugin
from patterns.monitor_object import CounterMonitor
from patterns.strategy import apply_strategy, double, square
from std.testing import assert_equal


def main() raises:
    var adapter = CelsiusAdapter(LegacyFahrenheitSensor(212))
    assert_equal(adapter.celsius(), 100)

    assert_equal(apply_strategy(5, double), 10)
    assert_equal(apply_strategy(5, square), 25)

    var kernel = Kernel()
    kernel.register("double", double_plugin)
    kernel.register("square", square_plugin)
    assert_equal(kernel.execute("double", 6), 12)
    assert_equal(kernel.execute("square", 6), 36)

    var counter = CounterMonitor()
    counter.increment(2)
    counter.increment(3)
    assert_equal(counter.current(), 5)

    print("mojo-pattern-sweep: 4/52 calibration passed")
