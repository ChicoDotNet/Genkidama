typedef PricingStrategy = int Function(int value);

int price(int value, PricingStrategy strategy) => strategy(value);

int regular(int value) => value;
int vip(int value) => value * 80 ~/ 100;

void verifyStrategy() {
  if (price(100, regular) != 100 || price(100, vip) != 80) {
    throw StateError('Strategy contract failed');
  }
}

void main() {
  verifyStrategy();
  print('regular=100;vip=80');
}
