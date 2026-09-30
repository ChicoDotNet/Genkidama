struct Model:
    var count: Int

    def __init__(out self, count: Int):
        self.count = count


struct ViewModel:
    var model: Model

    def __init__(out self, model: Model):
        self.model = model

    def increment(mut self):
        self.model.count += 1

    def display(self) -> Int:
        return self.model.count * 10


def run() -> Bool:
    var vm = ViewModel(Model(2))
    vm.increment()
    return vm.display() == 30
