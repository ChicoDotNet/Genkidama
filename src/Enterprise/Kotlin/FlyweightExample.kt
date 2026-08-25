data class TextStyle(val font: String, val size: Int, val color: String)

class StyleFactory {
    private val styles = mutableMapOf<String, TextStyle>()

    fun get(font: String, size: Int, color: String): TextStyle {
        val key = "$font|$size|$color"
        return styles.getOrPut(key) { TextStyle(font, size, color) }
    }

    val count: Int get() = styles.size
}

fun main() {
    val factory = StyleFactory()
    val red1 = factory.get("Inter", 12, "red")
    val red2 = factory.get("Inter", 12, "red")
    factory.get("Inter", 12, "blue")
    println("styles=${factory.count};shared=${red1 === red2};text=ABC")
}
