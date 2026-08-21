enum Node {
    File(i32),
    Folder(Vec<Node>),
}

impl Node {
    fn size(&self) -> i32 {
        match self {
            Self::File(bytes) => *bytes,
            Self::Folder(children) => children.iter().map(Self::size).sum(),
        }
    }
}

fn main() {
    let readme = Node::File(2);
    let docs = Node::Folder(vec![Node::File(3), Node::File(5)]);
    let root = Node::Folder(vec![Node::File(2), Node::Folder(vec![Node::File(3), Node::File(5)])]);

    println!("leaf={}", readme.size());
    println!("docs={}", docs.size());
    println!("root={}", root.size());
}
