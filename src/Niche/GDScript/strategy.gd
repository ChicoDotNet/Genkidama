extends SceneTree

func apply_pricing(amount: int, strategy: Callable) -> int:
    return strategy.call(amount)

func regular(amount: int) -> int:
    return amount

func vip(amount: int) -> int:
    return amount * 80 / 100

func campaign(amount: int) -> int:
    return amount - 25 if amount >= 100 else amount

func _initialize() -> void:
    assert(apply_pricing(100, regular) == 100, "regular strategy must preserve the amount")
    assert(apply_pricing(100, vip) == 80, "vip strategy must apply the discount")
    assert(apply_pricing(100, campaign) == 75, "campaign strategy must apply its threshold discount")
    assert(apply_pricing(80, campaign) == 80, "campaign strategy must preserve values below its threshold")
    print("GDScript Strategy: passed")
    quit()
