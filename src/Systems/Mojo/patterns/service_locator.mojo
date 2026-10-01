def email_service(value: Int) -> Int:
    return value + 10


def cache_service(value: Int) -> Int:
    return value + 20


def locate_and_call(service: Int, value: Int) -> Int:
    if service == 1:
        return email_service(value)
    return cache_service(value)


def run() -> Bool:
    return locate_and_call(1, 5) == 15 and locate_and_call(2, 5) == 25
