def peer_receive(peer: Int, message: Int) -> Int:
    return peer * 100 + message


def exchange(first: Int, second: Int, message: Int) -> Int:
    return peer_receive(first, message) + peer_receive(second, message)


def run() -> Bool:
    return exchange(1, 2, 5) == 310
