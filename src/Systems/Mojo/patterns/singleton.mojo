def singleton_id() -> Int:
    # A module-owned accessor exposes one logical instance identity.
    return 404


def run() -> Bool:
    return singleton_id() == singleton_id() and singleton_id() == 404
