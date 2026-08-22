final class ProcessRegistry {
    private int count;

    private ProcessRegistry() {}

    private static final class Holder {
        private static final ProcessRegistry INSTANCE = new ProcessRegistry();
    }

    static ProcessRegistry instance() {
        return Holder.INSTANCE;
    }

    void increment() {
        count++;
    }

    int count() {
        return count;
    }
}

public final class SingletonExample {
    public static void main(String[] args) {
        var first = ProcessRegistry.instance();
        var second = ProcessRegistry.instance();
        first.increment();
        System.out.println("same=" + (first == second));
        System.out.println("count=" + second.count());
    }
}
