object ProcessRegistry {
    var count: Int = 0
        private set

    fun increment() {
        count++
    }
}

fun main() {
    val first = ProcessRegistry
    val second = ProcessRegistry
    first.increment()
    println("same=${first === second}")
    println("count=${second.count}")
}
