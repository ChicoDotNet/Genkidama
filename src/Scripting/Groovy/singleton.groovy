final class Registry {
    private static final Registry INSTANCE = new Registry()
    private int count = 0

    private Registry() {}

    static Registry instance() {
        INSTANCE
    }

    void increment() {
        count++
    }

    int count() {
        count
    }
}

def first = Registry.instance()
def second = Registry.instance()
first.increment()

println "same=${first.is(second)}"
println "count=${second.count()}"
