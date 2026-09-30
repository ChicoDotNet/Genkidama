def television(command: Int) -> Int:
    return 100 + command


def radio(command: Int) -> Int:
    return 200 + command


struct Remote:
    var device: def(Int) thin -> Int

    def __init__(out self, device: def(Int) thin -> Int):
        self.device = device

    def press(self, command: Int) -> Int:
        return self.device(command)


def run() -> Bool:
    var tv_remote = Remote(television)
    var radio_remote = Remote(radio)
    return tv_remote.press(3) == 103 and radio_remote.press(3) == 203
