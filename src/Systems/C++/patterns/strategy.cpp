#include <functional>

static int price(int base, const std::function<int(int)>& strategy) {
    return strategy(base);
}

bool run() {
    const auto regular = [](int value) { return value; };
    const auto vip = [](int value) { return value * 80 / 100; };
    const auto campaign = [](int value) { return value >= 100 ? value - 25 : value; };

    return price(100, regular) == 100 &&
           price(100, vip) == 80 &&
           price(100, campaign) == 75 &&
           price(80, campaign) == 80;
}
