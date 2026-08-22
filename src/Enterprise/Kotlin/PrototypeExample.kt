data class ServiceProfile(
    var name: String,
    val features: MutableList<String>,
) {
    fun cloneProfile(): ServiceProfile = copy(features = features.toMutableList())

    fun describe(): String = "$name: ${features.joinToString(",")}" 
}

fun main() {
    val original = ServiceProfile("orders", mutableListOf("metrics"))
    val canary = original.cloneProfile()

    canary.name = "orders-canary"
    canary.features += "tracing"

    println("original=${original.describe()}")
    println("clone=${canary.describe()}")
}
