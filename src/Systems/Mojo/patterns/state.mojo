struct Context:
    var state: Int

    def __init__(out self):
        self.state = 0

    def request(mut self) -> Int:
        if self.state == 0:
            self.state = 1
            return 10
        self.state = 0
        return 20


def run() -> Bool:
    var context = Context()
    return context.request() == 10 and context.request() == 20 and context.state == 0
