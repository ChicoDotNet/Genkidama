def server(request: Int) -> Int:
    return request * 10


def client(request: Int) -> Int:
    return server(request)


def run() -> Bool:
    return client(7) == 70
