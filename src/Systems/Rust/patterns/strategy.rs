fn price<F>(base: i32, strategy: F) -> i32
where
    F: Fn(i32) -> i32,
{
    strategy(base)
}

pub fn run() -> bool {
    let regular = |value| value;
    let vip = |value| value * 80 / 100;
    let campaign = |value| if value >= 100 { value - 25 } else { value };

    price(100, regular) == 100
        && price(100, vip) == 80
        && price(100, campaign) == 75
        && price(80, campaign) == 80
}
