sealed trait Node {
  def size: Int
}

final case class FileLeaf(bytes: Int) extends Node {
  override def size: Int = bytes
}

final case class FolderComposite(children: List[Node]) extends Node {
  override def size: Int = children.map(_.size).sum
}

object Composite {
  def main(args: Array[String]): Unit = {
    val readme: Node = FileLeaf(2)
    val docs: Node = FolderComposite(List(FileLeaf(3), FileLeaf(5)))
    val root: Node = FolderComposite(List(readme, docs))

    println(s"leaf=${readme.size}")
    println(s"docs=${docs.size}")
    println(s"root=${root.size}")
  }
}
