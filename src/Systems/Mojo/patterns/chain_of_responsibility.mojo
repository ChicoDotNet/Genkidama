def faq_handler(amount: Int) -> Int:
    if amount <= 10:
        return 1
    return 0


def billing_handler(amount: Int) -> Int:
    if amount <= 500:
        return 2
    return 0


def fraud_handler(amount: Int) -> Int:
    return 3


def handle(amount: Int) -> Int:
    var result = faq_handler(amount)
    if result != 0:
        return result
    result = billing_handler(amount)
    if result != 0:
        return result
    return fraud_handler(amount)


def run() -> Bool:
    return handle(5) == 1 and handle(250) == 2 and handle(900) == 3
