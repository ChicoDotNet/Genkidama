from patterns.microkernel import Kernel, double_plugin, square_plugin
from std.testing import assert_equal, assert_raises, TestSuite


def test_registered_plugins_extend_the_kernel() raises:
    var kernel = Kernel()
    kernel.register("double", double_plugin)
    kernel.register("square", square_plugin)
    assert_equal(kernel.execute("double", 4), 8)
    assert_equal(kernel.execute("square", 4), 16)


def test_unknown_plugin_is_rejected() raises:
    var kernel = Kernel()
    with assert_raises():
        _ = kernel.execute("missing", 4)


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
