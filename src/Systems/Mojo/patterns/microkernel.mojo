from std.collections import Dict


def double_plugin(value: Int) -> Int:
    return value * 2


def square_plugin(value: Int) -> Int:
    return value * value


struct Kernel:
    var plugins: Dict[String, def(Int) thin -> Int]

    def __init__(out self):
        self.plugins = Dict[String, def(Int) thin -> Int]()

    def register(
        mut self, var name: String, plugin: def(Int) thin -> Int
    ):
        self.plugins[name^] = plugin

    def execute(self, name: String, value: Int) raises -> Int:
        return self.plugins[name](value)
