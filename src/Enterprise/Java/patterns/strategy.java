import java.util.function.IntUnaryOperator;

class PatternCell {
    private static int price(int base, IntUnaryOperator strategy) {
        return strategy.applyAsInt(base);
    }

    static boolean run() {
        IntUnaryOperator regular = value -> value;
        IntUnaryOperator vip = value -> value * 80 / 100;
        IntUnaryOperator campaign = value -> value >= 100 ? value - 25 : value;

        return price(100, regular) == 100
                && price(100, vip) == 80
                && price(100, campaign) == 75
                && price(80, campaign) == 80;
    }

    public static void main(String[] args) {
        if (!run()) {
            throw new AssertionError("Java Strategy contract failed");
        }
    }
}
