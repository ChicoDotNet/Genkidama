abstract interface class Node {
  int get size;
}

final class FileLeaf implements Node {
  FileLeaf(this.bytes);

  final int bytes;

  @override
  int get size => bytes;
}

final class FolderComposite implements Node {
  FolderComposite(this.children);

  final List<Node> children;

  @override
  int get size => children.fold(0, (total, child) => total + child.size);
}

void main() {
  final Node readme = FileLeaf(2);
  final Node docs = FolderComposite([FileLeaf(3), FileLeaf(5)]);
  final Node root = FolderComposite([readme, docs]);

  print('leaf=${readme.size}');
  print('docs=${docs.size}');
  print('root=${root.size}');
}
