abstract type Node end

struct FileLeaf <: Node
    bytes::Int
end

struct FolderComposite <: Node
    children::Vector{Node}
end

node_size(node::FileLeaf) = node.bytes
node_size(node::FolderComposite) = sum(node_size, node.children)

readme = FileLeaf(2)
docs = FolderComposite(Node[FileLeaf(3), FileLeaf(5)])
root = FolderComposite(Node[readme, docs])

println("leaf=$(node_size(readme))")
println("docs=$(node_size(docs))")
println("root=$(node_size(root))")
