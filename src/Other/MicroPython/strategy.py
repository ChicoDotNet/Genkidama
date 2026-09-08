def regular_strategy(price):
    return price


def vip_strategy(price):
    return price * 80 // 100


def campaign_strategy(price):
    if price < 100:
        return price
    return price * 75 // 100


def apply_strategy(price, strategy):
    if not callable(strategy):
        raise TypeError("strategy must be callable")
    return strategy(price)


def verify_strategy_canonical():
    assert apply_strategy(100, regular_strategy) == 100
    assert apply_strategy(100, vip_strategy) == 80
    assert apply_strategy(100, campaign_strategy) == 75
    assert apply_strategy(80, campaign_strategy) == 80

    try:
        apply_strategy(100, 42)
        raise AssertionError("non-callable strategy must fail")
    except TypeError:
        pass

    print("MicroPython Strategy: passed")


verify_strategy_canonical()
