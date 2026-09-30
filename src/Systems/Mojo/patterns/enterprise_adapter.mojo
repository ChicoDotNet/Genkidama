def legacy_customer_code(value: Int) -> Int:
    return value * 100


def canonical_customer_code(value: Int) -> Int:
    # Adapter translates the legacy scale into the enterprise contract.
    return legacy_customer_code(value) // 10


def run() -> Bool:
    return canonical_customer_code(7) == 70
