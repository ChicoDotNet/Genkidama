type
  NodeKind = enum
    nkFile, nkFolder

  Node = ref object
    case kind: NodeKind
    of nkFile:
      bytes: int
    of nkFolder:
      children: seq[Node]

proc fileNode(bytes: int): Node =
  Node(kind: nkFile, bytes: bytes)

proc folderNode(children: varargs[Node]): Node =
  Node(kind: nkFolder, children: @children)

proc size(node: Node): int =
  case node.kind
  of nkFile:
    result = node.bytes
  of nkFolder:
    for child in node.children:
      result += child.size()

let readme = fileNode(2)
let docs = folderNode(fileNode(3), fileNode(5))
let root = folderNode(readme, docs)

echo "leaf=", readme.size()
echo "docs=", docs.size()
echo "root=", root.size()
