abstract class Node
  abstract def size : Int32
end

class FileLeaf < Node
  def initialize(@bytes : Int32)
  end

  def size : Int32
    @bytes
  end
end

class FolderComposite < Node
  def initialize(@children : Array(Node))
  end

  def size : Int32
    @children.sum(&.size)
  end
end

readme = FileLeaf.new(2)
docs = FolderComposite.new([FileLeaf.new(3).as(Node), FileLeaf.new(5).as(Node)])
root = FolderComposite.new([readme.as(Node), docs.as(Node)])

puts "leaf=#{readme.size}"
puts "docs=#{docs.size}"
puts "root=#{root.size}"
