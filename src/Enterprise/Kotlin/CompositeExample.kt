interface Component {
    fun size(): Int
}

class FileLeaf(private val bytes: Int) : Component {
    override fun size(): Int = bytes
}

class FolderComposite(private vararg val children: Component) : Component {
    override fun size(): Int = children.sumOf { it.size() }
}

fun main() {
    val readme: Component = FileLeaf(2)
    val docs: Component = FolderComposite(FileLeaf(3), FileLeaf(5))
    val root: Component = FolderComposite(readme, docs)

    println("leaf=${readme.size()}")
    println("docs=${docs.size()}")
    println("root=${root.size()}")
}
