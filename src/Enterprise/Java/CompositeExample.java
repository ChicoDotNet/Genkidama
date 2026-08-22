import java.util.List;

interface Component {
    int size();
}

final class FileLeaf implements Component {
    private final int bytes;

    FileLeaf(int bytes) {
        this.bytes = bytes;
    }

    public int size() {
        return bytes;
    }
}

final class FolderComposite implements Component {
    private final List<Component> children;

    FolderComposite(Component... children) {
        this.children = List.of(children);
    }

    public int size() {
        return children.stream().mapToInt(Component::size).sum();
    }
}

public final class CompositeExample {
    public static void main(String[] args) {
        Component readme = new FileLeaf(2);
        Component docs = new FolderComposite(new FileLeaf(3), new FileLeaf(5));
        Component root = new FolderComposite(readme, docs);

        System.out.println("leaf=" + readme.size());
        System.out.println("docs=" + docs.size());
        System.out.println("root=" + root.size());
    }
}
