class Handler(private val name: String, private val limit: Int) {
    private var next: Handler? = null

    fun then(handler: Handler): Handler {
        next = handler
        return handler
    }

    fun handle(amount: Int, visited: MutableList<String>): String {
        visited.add(name)
        if (amount <= limit || next == null) {
            return name
        }
        return next!!.handle(amount, visited)
    }
}

fun main() {
    val faq = Handler("faq", 50)
    val billing = Handler("billing", 500)
    val escalation = Handler("escalation", Int.MAX_VALUE)
    faq.then(billing).then(escalation)

    val visited = mutableListOf<String>()
    val handled = faq.handle(250, visited)
    println("visited=${visited.joinToString(">")};handled=$handled;result=refund(250)")
}
