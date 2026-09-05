class PatternCell {
    enum GateState {
        LOCKED,
        UNLOCKED
    }

    enum Action {
        COIN,
        PUSH
    }

    static final class Turnstile {
        private GateState state = GateState.LOCKED;

        GateState state() {
            return state;
        }

        String apply(Action action) {
            return switch (state) {
                case LOCKED -> switch (action) {
                    case COIN -> {
                        state = GateState.UNLOCKED;
                        yield "unlocked";
                    }
                    case PUSH -> "blocked";
                };
                case UNLOCKED -> switch (action) {
                    case COIN -> "coin-returned";
                    case PUSH -> {
                        state = GateState.LOCKED;
                        yield "passed";
                    }
                };
            };
        }
    }

    private static void require(boolean condition) {
        if (!condition) {
            throw new AssertionError("Java State contract failed");
        }
    }

    public static void main(String[] args) {
        Turnstile gate = new Turnstile();
        require(gate.state() == GateState.LOCKED);

        require(gate.apply(Action.PUSH).equals("blocked"));
        require(gate.state() == GateState.LOCKED);

        require(gate.apply(Action.COIN).equals("unlocked"));
        require(gate.state() == GateState.UNLOCKED);

        require(gate.apply(Action.COIN).equals("coin-returned"));
        require(gate.state() == GateState.UNLOCKED);

        require(gate.apply(Action.PUSH).equals("passed"));
        require(gate.state() == GateState.LOCKED);

        System.out.println("java-state: passed");
    }
}
