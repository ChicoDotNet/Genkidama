#include <stdbool.h>

typedef int (*price_strategy)(int);

static int price(int base, price_strategy strategy) {
    return strategy(base);
}

static int regular(int value) {
    return value;
}

static int vip(int value) {
    return value * 80 / 100;
}

static int campaign(int value) {
    return value >= 100 ? value - 25 : value;
}

bool run(void) {
    return price(100, regular) == 100 &&
           price(100, vip) == 80 &&
           price(100, campaign) == 75 &&
           price(80, campaign) == 80;
}
